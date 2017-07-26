{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent.Async
import qualified Data.ByteString as B

main :: IO ()
main = do
    replicateConcurrently_ 500 $ do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet 8081 (tupleToHostAddress (127,0,0,1))
        send sock "GET /dist/index.js HTTP/1.1\r\n"
        send sock "Host: localhost\r\n"
        send sock "\r\n"
        recv sock 4096

