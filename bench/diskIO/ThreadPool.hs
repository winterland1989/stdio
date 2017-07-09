module Main where

import System.Posix.Internals (c_read, c_open, c_close, c_write, o_RDWR, o_CREAT, o_NONBLOCK)
import System.Posix.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import Data.Bits
import Data.IORef
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)

data Job = Job (MVar (IO Any)) (MVar Any)

newEmptyJob = do
    req <- newEmptyMVar
    res <- newEmptyMVar
    return (Job req res)

submitJob :: Job -> IO a -> IO a
submitJob (Job req res) io = do
    putMVar req (unsafeCoerce `fmap` io)
    unsafeCoerce `fmap` takeMVar res

startWorkerOn :: Int -> Job -> IO ThreadId
startWorkerOn cap (Job req res) = forkOn cap . forever $ do
        request <- takeMVar req
        r <- request
        putMVar res r

main :: IO ()
main = do
    [file] <- getArgs

    j1 <- newEmptyJob
    j2 <- newEmptyJob
    j3 <- newEmptyJob

    startWorkerOn 0 j1  -- For test purpose
    startWorkerOn 1 j2
    startWorkerOn 2 j3


    forConcurrently_ [0..100] $ \ i -> do
        let file' = file ++ "-" ++ show i
            job = case i `mod` 3 of 1 -> j1
                                    2 -> j2
                                    _ -> j3
        withCString file $ \ fp -> do
            withCString file' $ \ fp' -> submitJob job $ do
                fd <- c_open fp (o_RDWR .|. o_NONBLOCK) 0o666
                fd' <- c_open fp' (o_CREAT .|. o_RDWR .|. o_NONBLOCK) 0o666
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
