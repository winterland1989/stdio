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
    { uvmBlockTable  :: IORef (Array (MVar ()))     -- a array to store thread blocked on read or write

    , uvmFreeSlotList :: MVar [Int]                 -- we generate two unique range limited 'Int' /slot/
                                                    -- for each uv_handle_t(one for read and another for
                                                    -- write).
                                                    --
                                                    -- the slot is attached as the data field of
                                                    -- c struct uv_handle_t, thus will be available
                                                    -- to c callbacks, which are static functions:
                                                    -- inside callback we increase event counter and
                                                    -- push slot into event queue
                                                    --
                                                    -- after uv_run is finished, we read the counter
                                                    -- and the queue back, use the slots in the queue
                                                    -- as index to unblock thread in block queue
                                                    --
                                                    -- We also use this 'MVar' to do finalization of
                                                    -- I/O manager by attaching finalizers to it with 'mkWeakMVar'

    , uvmLoop        :: Ptr UVLoop                  -- the uv loop refrerence
    , uvmLoopData    :: Ptr UVLoopData              -- This is the pointer to uv_loop_t's data field

    , uvmLoopLock    :: MVar ()                     -- during uv_run this lock shall be held!
                                                    -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                    -- thus we can only add new event when uv_run is not
                                                    -- running, usually this is not a problem because
                                                    -- unsafe FFI can't run concurrently on one
                                                    -- capability, but with work stealing we'd better
                                                    -- ask for this lock before calling any uv APIs.

    , uvmIdleCounter :: Counter                     -- Counter for idle(no event) uv_runs, when counter
                                                    -- reaches 10, we start to increase waiting between
                                                    -- uv_run, until delay reach 8 milliseconds

    , uvmRunningLock :: MVar ()                     -- when manager is not running, it would be blocked
                                                    -- by this lock, use 'tryPutMVar' to wake up manager
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

-- | Start the uv loop on given capability
--
-- The thread will never block unlike the io manager in Mio, we take the golang poller's approach:
-- simply run non-block poll with a bound increasing delay, the reason is two-fold:
--
-- * libuv's APIs is generally not thread safe: you shouldn't add or remove events during uv_run.
-- Which makes safe blocking poll inconvenient, because doing that will requrie we have some thread
-- safe notification, and it's too complex.
--
-- * GHC thread is scheduled in magnitude of milliseconds, so the latency of polling, doing safe FFI
-- will save CPU when load is not very high, e.g. constantly
--
startUVManager :: UVManager -> Int -> IO ()
startUVManager uvm cap = do
  where
    loop block step = do
        (more, c) <- step block


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
        (more, c) <- step block


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
