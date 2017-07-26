{-# LANGUAGE BangPatterns #-}

{-|
Module      : System.IO.UVManager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

-}

module System.IO.UVManager where

import System.IO.UV
import System.IO.UVErrno
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Foreign
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Primitive.Addr
import System.IO.Unsafe
import System.Posix.Signals (sigVTALRM)

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable  :: IORef (Array (MVar ()))     -- a list to store blocked thread

    , uvmFreeSlotList :: MVar [Int]                 -- we generate an unique range limited 'Int' /slot/
                                                    -- for each uv_handle_t and uv_req_t

                                                    -- the slot is attached as the data field of
                                                    -- c struct such as handle and request,
                                                    -- thus it will be available to c callback
                                                    -- inside callback we increase event counter and
                                                    -- push slot into event queue
                                                    --
                                                    -- after uv_run is finished, we read the counter
                                                    -- and the queue back, use the slots in the queue
                                                    -- as index to unblock thread in block queue
                                                    --
                                                    -- during uv_run this lock shall be held!
                                                    --
                                                    -- We also use this 'MVar' to do finalization of
                                                    -- I/O manager by attaching finalizers to it with 'mkWeakMVar'


    , uvmLoopData    :: Ptr UVLoopData              -- This is the pointer we pass to uv_loop_t as
                                                    -- data field, it contains above fields
                                                    --
                                                    -- freed when free slot list run out of scope

    , uvmLoop        :: Ptr UVLoop                  -- the uv loop refrerence
                                                    --
                                                    -- close and freed when free slot list run out of scope

    , uvmRunning     :: IORef Bool                  -- is uv manager running?
    , uvmCap         :: Int                         -- the capability uv manager is runnig on
    , uvmBlockTimer  :: Ptr UVHandle
    }

initTableSize :: Int
initTableSize = 128

uvManager :: IORef (Array UVManager)
uvManager = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    forM [0..numCaps-1] $ \ i -> do
        writeArr uvmArray i =<< newUVManager initTableSize i
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

getUVManager :: IO UVManager
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManager
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

newUVManager :: Int -> Int -> IO UVManager
newUVManager siz cap = do

    blockTable <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTable i =<< newEmptyMVar
    iBlockTable <- unsafeFreezeArr blockTable
    iBlockTableRef <- newIORef iBlockTable

    freeSlotList <- newMVar [0..siz-1]

    readBufferTable <- mallocBytes siz'
    readBufferSizeTable <- mallocBytes siz''
    writeBufferTable <- mallocBytes siz'
    writeBufferSizeTable <- mallocBytes siz''
    resultTable <- mallocBytes siz''

    eventQueue <- mallocBytes siz''

    uvLoop <- malloc
    uvLoopData <- malloc

    uv_loop_init uvLoop                 -- init will clear the data field, so init first

    poke uvLoop (UVLoop uvLoopData)
    poke uvLoopData (UVLoopData 0
        eventQueue readBufferTable readBufferSizeTable
        writeBufferTable writeBufferSizeTable resultTable)

    uvmRunning <- newIORef False

    blockTimer <- mallocBytes . fromIntegral  =<< uv_handle_size (getUVHandleType uV_TIMER)
    uv_timer_init uvLoop blockTimer

    _ <- mkWeakMVar freeSlotList $ do
        r <- uv_loop_close uvLoop
        when (UVErrno r /= uV_EBUSY) $ do -- TODO: retry
            free uvLoop
            free uvLoopData
            free readBufferTable
            free readBufferSizeTable
            free writeBufferTable
            free writeBufferSizeTable
            free resultTable
            free eventQueue
            free blockTimer

    return (UVManager iBlockTableRef freeSlotList uvLoopData uvLoop uvmRunning cap blockTimer)

  where
    siz' = siz * sizeOf (undefined :: Ptr ())
    siz'' = siz * sizeOf (undefined :: CSize)

ensureUVMangerRunning :: UVManager -> IO ()
ensureUVMangerRunning uvm = do
    isRunning <- readIORef (uvmRunning uvm)
    unless isRunning $ do
        forkOn (uvmCap uvm) $ loop False
            (step (uvmFreeSlotList uvm) (uvmLoopData uvm) (uvmLoop uvm) (uvmBlockTable uvm) (uvmBlockTimer uvm))
        return ()
  where
    loop block step = do
        yield
        (r, c) <- step block
        when (r > 0) $ loop (c == 0) step

    step :: MVar [Int] -> Ptr UVLoopData -> Ptr UVLoop -> IORef (Array (MVar ())) -> Ptr UVHandle -> Bool -> IO (CInt, CSize)
    step freeSlotList uvLoopData uvLoop iBlockTableRef blockTimer block = do
        withMVar freeSlotList $ \ _ -> do             -- now let's begin
            clearUVLoopuEventCounter uvLoopData
            hs_timer_start_no_callback blockTimer 10                     -- a temp walkaround to reduce CPU usage
            r <- if block then uv_run_safe uvLoop uV_RUN_ONCE       -- when we wait for some long block event
                          else uv_run uvLoop uV_RUN_NOWAIT
            -- TODO, handle exception
            (c, q) <- peekEventQueue uvLoopData
            blockTable <- readIORef iBlockTableRef
            forM_ [0..(fromIntegral c-1)] $ \ i -> do       -- convert CSize to int first, otherwise (c-1) may overflow
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                tryPutMVar lock ()
            return (r, c)

allocSlot :: UVManager -> IO Int
allocSlot (UVManager blockTableRef freeSlotList uvLoopData uvLoop _ _ _) = do
    modifyMVar freeSlotList $ \ freeList -> case freeList of
        (s:ss) -> return (ss, s)
        []     -> do        -- free list is empty, we double it
            (UVLoopData eventCounter eventQueue readBufferTable readBufferSizeTable
                writeBufferTable writeBufferSizeTable resultTable) <- peek uvLoopData  -- peek old data back

            blockTable <- readIORef blockTableRef
            let oldSiz = sizeofArr blockTable
                siz = oldSiz * 2
                siz' = siz * sizeOf (undefined :: Ptr ())
                siz'' = siz * sizeOf (undefined :: CSize)

            blockTable' <- newArr siz
            copyArr blockTable' 0 blockTable 0 oldSiz
            forM_ [oldSiz..siz-1] $ \ i ->
                writeArr blockTable' i =<< newEmptyMVar
            !iBlockTable' <- unsafeFreezeArr blockTable'
            writeIORef blockTableRef iBlockTable'

            readBufferTable' <- reallocBytes readBufferTable siz'
            readBufferSizeTable' <- reallocBytes readBufferSizeTable siz''
            writeBufferTable' <- reallocBytes writeBufferTable siz'
            writeBufferSizeTable' <- reallocBytes writeBufferSizeTable siz''
            resultTable' <- reallocBytes resultTable siz''

            eventQueue' <- reallocBytes eventQueue siz''

            poke uvLoopData $ UVLoopData eventCounter
                eventQueue' readBufferTable' readBufferSizeTable'
                writeBufferTable' writeBufferSizeTable' resultTable'

            return ([oldSiz+1..siz-1], oldSiz)    -- fill the free slot list

freeSlot :: Int -> UVManager -> IO ()
freeSlot slot  (UVManager _ freeSlotList _ _ _ _ _) =
    modifyMVar_ freeSlotList $ \ freeList -> return (slot:freeList)



