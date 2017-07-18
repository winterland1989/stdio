module Main where

import System.Posix.Internals (c_read, c_open, c_close, c_write, o_RDWR, o_CREAT, o_NONBLOCK)
import System.Posix.Internals (c_safe_read, c_safe_open, c_close, c_safe_write, o_RDWR, o_CREAT, o_NONBLOCK)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent (getNumCapabilities)
import System.Environment
import Data.Bits
import Data.IORef.Unboxed
import System.IO.Unsafe

unsafeCounter :: Counter
unsafeCounter = unsafePerformIO $ do newCounter 0
{-# NOINLINE unsafeCounter #-}

main :: IO ()
main = do
    [file] <- getArgs
    forConcurrently_ [0..100] $ \ i -> do
        let file' = file ++ "-" ++ show i
        withCString file $ \ fp -> do
            withCString file' $ \ fp' -> do
                fd <- c_open fp (o_RDWR .|. o_NONBLOCK) 0o666
                fd' <- c_open fp' (o_CREAT .|. o_RDWR .|. o_NONBLOCK) 0o666
                loop fd fd'
                c_close fd
                c_close fd'

  where
    loop fd fd' = do
        ptr <- mallocBytes 32750
        siz <- do
            uc <- readIORefU unsafeCounter
            tc <- getNumCapabilities
            if uc >= tc `div` 2
            then do
                c_safe_read fd ptr 32750
            else do
                atomicAddCounter unsafeCounter 1
                siz <- c_read fd ptr 32750
                atomicSubCounter unsafeCounter 1
                return siz

        loopWrite fd' ptr (fromIntegral siz)
        free ptr
        case siz `compare` 0 of
            LT -> error ("error:" ++ show siz)
            EQ -> return ()
            GT -> loop fd fd'


    loopWrite fd ptr n = do
        siz <- do
            uc <- readIORefU unsafeCounter
            tc <- getNumCapabilities
            if uc >= tc `div` 2
            then fromIntegral `fmap` c_safe_write fd ptr n
            else do
                atomicAddCounter unsafeCounter 1
                siz <- fromIntegral `fmap` c_write fd ptr n
                atomicSubCounter unsafeCounter 1
                return siz

        case siz `compare` n of
            LT -> loopWrite fd ptr (n - siz)
            EQ -> return ()
            GT -> error ("over write:" ++ show siz)
