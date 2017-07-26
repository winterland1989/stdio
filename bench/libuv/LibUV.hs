{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO.Handle
import System.IO.TCP
import System.IO.UV
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent.Async
import Foreign
import qualified Data.Vector as V

main :: IO ()
main = do
    replicateConcurrently_ 500 $ do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet 8081 (tupleToHostAddress (127,0,0,1))
        send sock "GET /dist/index.js HTTP/1.1\r\n"
        send sock "Host: localhost\r\n"
        send sock "\r\n"
        let fd = fdSocket sock
        tcp <- newTCP fd
        h <- newInputHandle tcp 4096
        readHandle h








