{-# LANGUAGE BangPatterns #-}
module System.LowResTimer
  ( -- * low resolution timers
    registerLowResTimer
  ) where

{-|
Module      : System.LowResTimer
Description : Low resolution (1s) timing wheel
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide low resolution (1s) timers using a single timing wheel of size 128, the timer thread will automatically
started or stopped based on demannd. register or cancel a timeout is O(1), and each step only need scan n/128 items given
timers are registered in an even fashion.

-}

import Data.Array
import Data.Primitive.MutVar
import GHC.Event
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.IORef.Unboxed
import Data.IORef
import Data.Word
import qualified Control.Exception as E

queueSize :: Int
queueSize = 128

-- | A simple timing wheel
--
data TimerList = TimerItem {-# UNPACK #-} !Counter (IO ()) TimerList | TimerNil

data LowResTimerManager = LowResTimerManager
    { lrTimerQueue :: Array (IORef TimerList)
    , lrIndexLock :: MVar Int
    , lrNewRegisterCount :: Counter
    , lrRunningLock :: MVar Bool
    }

lowResTimerManager :: IORef LowResTimerManager
{-# NOINLINE lowResTimerManager #-}
lowResTimerManager = unsafePerformIO $ do
    indexLock <- newMVar 0
    rcount <- newCounter 0
    runningLock <- newMVar False
    queue <- newArr queueSize
    forM [0..queueSize-1] $ \ i -> do
        writeArr queue i =<< newIORef TimerNil
    iqueue <- unsafeFreezeArr queue
    newIORef (LowResTimerManager iqueue indexLock rcount runningLock)

getSystemLowResTimerManager :: IO LowResTimerManager
getSystemLowResTimerManager = readIORef lowResTimerManager


-- | Register a new timer, start timing wheel if it's not turning.
--
-- If the action could block, you may want to run it in another thread. Example to kill a thread after 10s:
--
-- @
--   registerLowResTimer 10 (forkIO $ killThread tid)
-- @
--
registerLowResTimer :: Int          -- ^ timout in seconds
                    -> IO ()        -- ^ the action you want to perform, it should not block
                    -> IO (IO ())   -- ^ cancel action
registerLowResTimer t action = do
    lrtm@(LowResTimerManager queue indexLock rcount _) <- getSystemLowResTimerManager

    let (round, tick) = (max 0 t) `quotRem` queueSize
    i <- readMVar indexLock
    tlistRef <- indexArrM queue ((i + tick) `rem` queueSize)
    tlist <- readIORef tlistRef
    roundCounter <- newCounter round
    let newList = TimerItem roundCounter action tlist

    E.mask_ $ do
        atomicModifyIORef' tlistRef ( \ _ ->  (newList, ()) )
        atomicAddCounter rcount 1

    ensureLowResTimerManager lrtm

    return (void $ atomicOrCounter roundCounter (-1))

-- | Check if low resolution timer manager loop is running, start loop if not.
--
ensureLowResTimerManager :: LowResTimerManager -> IO ()
ensureLowResTimerManager lrtm@(LowResTimerManager _ _ _ runningLock) = do
    modifyMVar_ runningLock $ \ running -> do
        unless running (void . forkIO $ threadDelay 1000 >> print "time manager started" >> startLowResTimerManager lrtm)
        return True

-- | Start low resolution timer loop, the loop is automatically stopped if there's no more new registrations.
--
startLowResTimerManager :: LowResTimerManager ->IO ()
startLowResTimerManager lrtm@(LowResTimerManager _ _ rcount runningLock)  = do
    modifyMVar_ runningLock $ \ _ -> do
        c <- readIORefU rcount
        if c > 0
        then do
            forkIO (fireLowResTimerQueue lrtm)  -- we offload the scanning to another thread to minimize
                                                -- the time we holding runningLock
            htm <- getSystemTimerManager
            registerTimeout htm 1000000 (startLowResTimerManager lrtm)
            return True
        else do
            print "timer manager stoped"
            return False -- if we haven't got any registered timeout, we stop the time manager
                         -- doing this can stop us from getting the way of idle GC
                         -- since we're still inside runningLock, we won't miss new registration.

-- | Scan the timeout queue in current tick index, and move tick index forward by one.
--
fireLowResTimerQueue :: LowResTimerManager -> IO ()
fireLowResTimerQueue lrtm@(LowResTimerManager queue indexLock rcount runningLock) = do
    (tList, tListRef) <- modifyMVar indexLock $ \ index -> do
        tListRef <- indexArrM queue index
        tList <- atomicModifyIORef' tListRef $ \ tList -> (TimerNil, tList)
        let !index' = (index+1) `rem` queueSize
        return (index', (tList, tListRef))

    go tList tListRef rcount
  where
    go (TimerItem roundCounter action nextList) tListRef rcount = do
        r <- atomicSubCounter_ roundCounter 1
        case r `compare` 0 of
            LT -> do
                atomicSubCounter rcount 1
                go nextList tListRef rcount
            EQ -> do
                atomicSubCounter rcount 1
                action
                go nextList tListRef rcount
            GT -> do
                atomicModifyIORef' tListRef $ \ tlist -> (TimerItem roundCounter action tlist, () )
                go nextList tListRef rcount
    go TimerNil _ _ = return ()


