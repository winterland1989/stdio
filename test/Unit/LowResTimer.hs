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
        testCaseSteps "timers registration should not be missed" $ \ step -> do
            step "forking threads and register 1000000 timers"
            c <- newCounter 0
            replicateConcurrently_ 10000 $ do
                forM_ [1..10] $ \ i -> do
                    registerLowResTimer i (void $ atomicAddCounter c 1)

            threadDelay 11000000 -- make sure all timers are fired
            c' <- readIORefU c
            assertEqual "" 100000 c'
    ]
