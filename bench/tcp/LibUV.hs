{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO.Net
import System.IO.Buffered
import Control.Concurrent
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import Control.Monad
import System.IO.Exception
import System.IO.UV.Stream
import System.IO
import Data.IORef.Unboxed

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    acceptCounter <- newCounter 0
    startCounter <- newCounter 0
    readCounter <- newCounter 0
    finishCounter <- newCounter 0
    let conf = ServerConfig
            (SockAddrInet 8888 inetAny)
            128
            (\ uvs ->do
                atomicAddCounter acceptCounter 1
                echo startCounter readCounter finishCounter uvs)
            (print :: SomeException -> IO())
            (print :: SomeException -> IO())

    forkIO $ do
        threadDelay 4000000
        print =<< readIORefU acceptCounter
        print =<< readIORefU startCounter
        print =<< readIORefU readCounter
        print =<< readIORefU finishCounter
    startServer conf
  where
    echo startCounter readCounter finishCounter uvs = do
        atomicAddCounter startCounter 1
        recvbuf <- mallocPlainForeignPtrBytes 2048
        r <- withForeignPtr recvbuf $ \ p -> do
            readInput uvs p 2048
        atomicAddCounter readCounter 1
        if (r /= 0)
        then do

            let (B.PS sendbuffp _ l) = sendbuf
            withForeignPtr sendbuffp $ \ p ->
                writeOutput uvs p l

            echo startCounter readCounter finishCounter uvs
        else void $ atomicAddCounter finishCounter 1

    sendbuf =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `B.append` (B.replicate 500 48)




