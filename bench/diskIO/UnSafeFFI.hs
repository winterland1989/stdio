{-# LANGUAGE CPP #-}
module Main where

import System.Posix.Internals (c_read, c_open, c_close, c_write, o_RDWR, o_CREAT, o_NONBLOCK)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.Async (forConcurrently_)
import System.Environment
import Data.Bits
#if defined(mingw32_HOST_OS)
import Foreign.Ptr (castPtr)
#endif

main :: IO ()
main = do
    [file] <- getArgs
    forConcurrently_ [0..100] $ \ i -> do
        let file' = file ++ "-" ++ show i
        withCString file $ \ fp -> do
            withCString file' $ \ fp' -> do
#if defined(mingw32_HOST_OS)
                fd <- c_open (castPtr fp) (o_RDWR .|. o_NONBLOCK) 0o666
                fd' <- c_open (castPtr fp') (o_CREAT .|. o_RDWR .|. o_NONBLOCK) 0o666
#else
                fd <- c_open fp (o_RDWR .|. o_NONBLOCK) 0o666
                fd' <- c_open fp' (o_CREAT .|. o_RDWR .|. o_NONBLOCK) 0o666
#endif
                loop fd fd'
                c_close fd
                c_close fd'

  where
    loop fd fd' = do
        ptr <- mallocBytes 32750
        siz <- c_read fd ptr 32750
        loopWrite fd' ptr (fromIntegral siz)
        free ptr
        case siz `compare` 0 of
            LT -> error ("error:" ++ show siz)
            EQ -> return ()
            GT -> loop fd fd'


    loopWrite fd ptr n = do
        siz <- fromIntegral `fmap` c_write fd ptr n
        case siz `compare` n of
            LT -> loopWrite fd ptr (n - siz)
            EQ -> return ()
            GT -> error ("over write:" ++ show siz)
