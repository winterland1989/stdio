module Unit.LowResTimer where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Data.IORef.Unboxed
import System.LowResTimer

unitLowResTimer :: TestTree
unitLowResTimer = testGroup "low resolution timers" [
        testCase "timers registration should not be missed" $ do
            c <- newCounter 0
            replicateConcurrently_ 10000 $ do
                forM_ [1..10] $ \ i -> do
                    registerLowResTimer i (void $ atomicAddCounter c 1)
            threadDelay 1100000 -- make sure all timers are fired
            c' <- readIORefU c
            assertEqual "timers registration counter" 100000 c'

    ,   testCase "debounce sh" $ do
            c <- newCounter 0
            debouncedAdd <- debounce 1 (atomicAddCounter c 1)
            forkIO . replicateM_ 10000 $ do
                debouncedAdd
                threadDelay 500
            threadDelay 1000000  -- wait 1s here
            c' <- readIORefU c
            assertBool "debounced add" (5  <= c' && c' <= 6)
    ]
