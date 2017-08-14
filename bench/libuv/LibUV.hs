{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO.Handle
import System.IO.Socket.Base
import System.IO.Socket.Address
import Control.Concurrent
import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import Control.Monad
import Data.IORef.Unboxed
import Network.Socket (withSocketsDo)
import System.IO

main :: IO ()
main = withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    sock <- socket aF_INET sOCK_STREAM iPPROTO_TCP
    b <- bind sock $ SockAddrInet (fromIntegral 8888) inetAny
    l <- listen b 32768
    cap <- getNumCapabilities
    capCounter <- newCounter 0
    forever $ do
        sock' <- accept l
        c <- atomicAddCounter_ capCounter 1
        forkOn c $ do
            tcp <- newTCP sock'
            forever $ do
                recvbuf <- mallocPlainForeignPtrBytes 2048
                withForeignPtr recvbuf $ \ p -> do
                    _ <- readInput tcp p 2048
                    return ()

                let (B.PS sendbuffp _ l) = sendbuf
                withForeignPtr sendbuffp $ \ p ->
                    writeOutput tcp p l

  where
    sendbuf =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `B.append` (B.replicate 500 48)




