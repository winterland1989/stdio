{-# LANGUAGE BangPatterns #-}

{-|
Module      : System.IO.UV.Manager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

-}

module System.IO.UV.Manager where

import System.IO.UV.FFI
import System.IO.UV.Exception
import GHC.Stack.Compat
import qualified System.IO.Exception as E
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

newUVManager :: HasCallStack => Int -> Int -> IO UVManager
newUVManager siz cap = do

    blockTable <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTable i =<< newEmptyMVar
    iBlockTable <- unsafeFreezeArr blockTable
    iBlockTableRef <- newIORef iBlockTable

    freeSlotList <- newMVar [0..siz-1]

    uvLoop <- E.throwOOMIfNull callStack "malloc loop and data for uv manager" $ hs_loop_init (fromIntegral siz)

    uvLoopData <- peek_uv_loop_data uvLoop

    uvmRunning <- newIORef False

    blockTimer <- E.throwOOMIfNull callStack "malloc block timer for uv manager" $ hs_handle_init uV_TIMER
    uv_timer_init uvLoop blockTimer

    _ <- mkWeakMVar freeSlotList $ do
        hs_handle_close blockTimer
        hs_loop_close uvLoop

    return (UVManager iBlockTableRef freeSlotList uvLoopData uvLoop uvmRunning cap blockTimer)

ensureUVMangerRunning :: UVManager -> IO ()
ensureUVMangerRunning uvm = do
    isRunning <- readIORef (uvmRunning uvm)
    unless isRunning $ do
        forkOn (uvmCap uvm) $ do
            loop False
                (step uvm)
        return ()
  where
    loop block step = do
        yield
        (more, c) <- step block
        when more $ loop (c == 0) step

    step :: UVManager -> Bool -> IO (Bool, CSize)
    step (UVManager iBlockTableRef freeSlotList uvLoopData uvLoop uvmRunning _ blockTimer) block = do
        withMVar freeSlotList $ \ _ -> do             -- now let's begin
            blockTable <- readIORef iBlockTableRef
            let siz = sizeofArr blockTable
            clearUVLoopuEventCounter uvLoopData
            r <- if block
                then do
                    hs_timer_start_stop_loop blockTimer 10 -- start a timer to end the blocking wait for non-threaded RTS
                    uv_run_safe uvLoop uV_RUN_ONCE
                else uv_run uvLoop uV_RUN_NOWAIT
            -- TODO, handle exception
            (c, q) <- peekEventQueue uvLoopData
            forM_ [0..(fromIntegral c-1)] $ \ i -> do      -- convert CSize to int first, otherwise (c-1) may overflow
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                tryPutMVar lock ()
                return ()
            when (r == 0) (writeIORef uvmRunning False)
            return (r /= 0, c)

allocSlot :: UVManager -> IO Int
allocSlot (UVManager blockTableRef freeSlotList uvLoopData uvLoop _ _ _) = do
    modifyMVar freeSlotList $ \ freeList -> case freeList of
        (s:ss) -> return (ss, s)
        []     -> do        -- free list is empty, we double it

            blockTable <- readIORef blockTableRef
            let oldSiz = sizeofArr blockTable
                newSiz = oldSiz * 2
            blockTable' <- newArr newSiz
            copyArr blockTable' 0 blockTable 0 oldSiz
            forM_ [oldSiz..newSiz-1] $ \ i ->
                writeArr blockTable' i =<< newEmptyMVar
            !iBlockTable' <- unsafeFreezeArr blockTable'
            writeIORef blockTableRef iBlockTable'

            hs_loop_resize uvLoop (fromIntegral newSiz)

            return ([oldSiz+1..newSiz-1], oldSiz)    -- fill the free slot list

freeSlot :: Int -> UVManager -> IO ()
freeSlot slot  (UVManager _ freeSlotList _ _ _ _ _) =
    modifyMVar_ freeSlotList $ \ freeList -> return (slot:freeList)



