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

import GHC.Stack.Compat
import qualified System.IO.UV.Exception as E
import qualified System.IO.Exception as E
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
import Foreign hiding (void)
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Primitive.Addr
import System.IO.Unsafe
import System.IO.UV.Base

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTableR  :: IORef (Array (MVar ()))     -- a list to store blocked thread
    , uvmBlockTableW  :: IORef (Array (MVar ()))     -- a list to store blocked thread

    , uvmFreeSlotList :: MVar [Int]                -- we generate an unique range limited 'Int' /slot/
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


    , uvmLoopData    :: Ptr UVLoopData              -- This is the pointer to uv_loop_t's data field
                                                    --
    , uvmLoop        :: Ptr UVLoop                  -- the uv loop refrerence

    , uvmState       :: Counter                     -- 0: stopped, 1: running, -1: blocking
    , uvmCap         :: Int                         -- the capability uv manager is runnig on
    , uvmBlockTimer  :: Ptr UVHandle
    , uvmBlockAsync  :: Ptr UVHandle
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

    blockTableR <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTableR i =<< newEmptyMVar
    iBlockTableR <- unsafeFreezeArr blockTableR
    iBlockTableRRef <- newIORef iBlockTableR

    blockTableW <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTableW i =<< newEmptyMVar
    iBlockTableW <- unsafeFreezeArr blockTableW
    iBlockTableWRef <- newIORef iBlockTableW

    freeSlotList <- newMVar [1..siz]

    uvLoop <- E.throwOOMIfNull callStack "malloc loop and data for uv manager" $ hs_loop_init (fromIntegral siz)

    uvLoopData <- peek_uv_loop_data uvLoop

    uvmState <- newCounter 0

    blockTimer <- E.throwOOMIfNull callStack "malloc block timer for uv manager" $ hs_handle_init uV_TIMER
    uv_timer_init uvLoop blockTimer

    blockAsync <- E.throwOOMIfNull callStack "malloc block async handler for uv manager" $ hs_handle_init uV_ASYNC
    hs_async_init_stop_loop uvLoop blockAsync

    _ <- mkWeakMVar freeSlotList $ do
        hs_handle_close blockTimer
        hs_handle_close blockTimer
        hs_loop_close uvLoop

    return (UVManager iBlockTableRRef iBlockTableWRef freeSlotList uvLoopData uvLoop uvmState cap blockTimer blockAsync)

wakeUpBlockingUVManager :: UVManager -> IO ()
wakeUpBlockingUVManager uvm = do
    E.throwUVErrorIfMinus $ uv_async_send (uvmBlockAsync uvm)

ensureUVMangerRunning :: HasCallStack => UVManager -> IO ()
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
        if more then loop (c == 0) step
                else writeIORef (uvmRunning uvm) False

    step :: UVManager -> Bool -> IO (Bool, CSize)
    step (UVManager iBlockTableRRef iBlockTableWRef freeSlotList uvLoopData uvLoop uvmRunning uvmBlocking _ blockTimer _) block = do
        withMVar uvmRunning $ \ _ -> do             -- now let's begin
            blockTableR <- readIORef iBlockTableRRef
            blockTableW <- readIORef iBlockTableWRef
            let siz = sizeofArr blockTableR
            clearUVLoopuEventCounter uvLoopData
            (c, q) <- peekEventQueue uvLoopData
            r <- if block
                then do
                    unless rtsSupportsBoundThreads
                        (void . E.throwUVErrorIfMinus callStack "start timer to unblock uv loop for non-threaded RTS" $
                            hs_timer_start_stop_loop blockTimer 1)
                    E.throwUVErrorIfMinus callStack "blocking uv loop on threaded RTS" $ do
                            uv_run_safe uvLoop uV_RUN_ONCE
                else E.throwUVErrorIfMinus callStack "non-blocking uv loop on threaded RTS" $
                        uv_run uvLoop uV_RUN_NOWAIT
            -- TODO, handle exception
            (c, q) <- peekEventQueue uvLoopData
            forM_ [0..(fromIntegral c-1)] $ \ i -> do      -- convert CSize to int first, otherwise (c-1) may overflow
                slot <- peekElemOff q i
                lock <-
                    if slot >= 0
                    then indexArrM blockTableR (fromIntegral slot)
                    else indexArrM blockTableW (fromIntegral slot)
                tryPutMVar lock ()
                return ()
            return (r /= 0, c)

allocSlot :: UVManager -> IO Int
allocSlot (UVManager blockTableRRef blockTableWRef freeSlotList uvLoopData uvLoop _ _ _ _) = do
    modifyMVar freeSlotList $ \ freeList -> case freeList of
        (s:ss) -> return (ss, s)
        []     -> do        -- free list is empty, we double it

            blockTableR <- readIORef blockTableRRef
            blockTableW <- readIORef blockTableWRef
            let oldSiz = sizeofArr blockTableR
                newSiz = oldSiz * 2
            blockTableR' <- newArr newSiz
            copyArr blockTableR' 0 blockTableR 0 oldSiz
            forM_ [oldSiz..newSiz-1] $ \ i ->
                writeArr blockTableR' i =<< newEmptyMVar
            !iBlockTableR' <- unsafeFreezeArr blockTableR'
            writeIORef blockTableRRef iBlockTableR'

            blockTableW' <- newArr newSiz
            copyArr blockTableW' 0 blockTableW 0 oldSiz
            forM_ [oldSiz..newSiz-1] $ \ i ->
                writeArr blockTableW' i =<< newEmptyMVar
            !iBlockTableW' <- unsafeFreezeArr blockTableW'
            writeIORef blockTableWRef iBlockTableW'
            hs_loop_resize uvLoop (fromIntegral newSiz)

            return ([oldSiz+2..newSiz], oldSiz+1)    -- fill the free slot list

freeSlot :: Int -> UVManager -> IO ()
freeSlot slot  (UVManager _ _ freeSlotList _ _ _ _ _ _) =
    modifyMVar_ freeSlotList $ \ freeList -> return (slot:freeList)
