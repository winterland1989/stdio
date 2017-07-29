{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO.Handle
import System.IO.TCP
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent.Async
import Foreign
import qualified Data.Vector as V
import Data.Word

main :: IO ()
main = do
    replicateConcurrently_ 200 $ do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet 80 (tupleToHostAddress (220,181,112,244))
        send sock "GET / HTTP/1.1\r\n"
        send sock "Host: www.baidu.com\r\n"
        send sock "\r\n"
        let fd = fdSocket sock
        tcp <- newTCP fd
        h <- newInputHandle tcp 4096
        readHandle h








