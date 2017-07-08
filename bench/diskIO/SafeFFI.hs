module Main where

import System.Posix.Internals (c_safe_read, c_safe_open, c_close, c_safe_write, o_RDWR, o_CREAT, o_NONBLOCK)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.Async (forConcurrently_)
import System.Environment
import Data.Bits

main :: IO ()
main = do
    [file] <- getArgs
    forConcurrently_ [0..100] $ \ i -> do
        let file' = file ++ "-" ++ show i
        withCString file $ \ fp -> do
            withCString file' $ \ fp' -> do
                fd <- c_safe_open fp (o_RDWR .|. o_NONBLOCK) 0o666
                fd' <- c_safe_open fp' (o_CREAT .|. o_RDWR .|. o_NONBLOCK) 0o666
                loop fd fd'
                c_close fd
                c_close fd'

  where
    loop fd fd' = do
        ptr <- mallocBytes 1024
        siz <- c_safe_read fd ptr 1024
        loopWrite fd' ptr 1024
        free ptr
        case siz `compare` 0 of
            LT -> error ("error:" ++ show siz)
            EQ -> return ()
            GT -> loop fd fd'


    loopWrite fd ptr n = do
        siz <- fromIntegral `fmap` c_safe_write fd ptr n
        case siz `compare` n of
            LT -> loopWrite fd ptr (n - siz)
            EQ -> return ()
            GT -> error ("over write:" ++ show siz)
