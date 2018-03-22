{-|
Module      : System.IO.Net
Description : TCP or IPC servers and clients
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating TCP or IPC servers and clients. IPC Support is implemented with named pipes on Windows, and UNIX domain sockets on other operating systems.

On UNIX, the local domain is also known as the UNIX domain. The path is a filesystem path name. It gets truncated to sizeof(sockaddr_un.sun_path) - 1, which varies on different operating system between 91 and 107 bytes. The typical values are 107 on Linux and 103 on macOS. The path is subject to the same naming conventions and permissions checks as would be done on file creation. It will be visible in the filesystem, and will persist until unlinked.

On Windows, the local domain is implemented using a named pipe. The path must refer to an entry in \\?\pipe\ or \\.\pipe\. Any characters are permitted, but the latter may do some processing of pipe names, such as resolving .. sequences. Despite appearances, the pipe name space is flat. Pipes will not persist, they are removed when the last reference to them is closed. Do not forget JavaScript string escaping requires paths to be specified with double-backslashes, such as:

net.createServer().listen(
  path.join('\\\\?\\pipe', process.cwd(), 'myctl'));

-}

module System.IO.Net (
    UVStream
  , connection
  , connect
  , module System.IO.Net.SockAddr
  ) where


import System.IO.Net.SockAddr
import System.IO.Exception
import System.IO.Buffer
import System.IO.UV.Manager
import System.IO.UV.Stream
import System.IO.UV.Internal
import Control.Concurrent.MVar
import Foreign.Ptr
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class


uvTCP :: HasCallStack => UVManager -> Resource (Ptr UVHandle)
uvTCP = uvHandle uV_TCP (\ loop handle -> uv_tcp_init loop handle >> return handle)

connection :: HasCallStack => Resource UVStream
connection = do
    uvm <- liftIO getUVManager
    rslot <- uvSlot uvm
    wslot <- uvSlot uvm
    handle <- uvTCP uvm
    req <- uvReq uV_WRITE
    liftIO $ do
        pokeUVHandleData handle rslot
        pokeUVReqData req wslot
        return (UVStream handle rslot req wslot uvm)


connect :: HasCallStack
    => SockAddr
    -> Maybe SockAddr
    -> Resource UVStream
connect target local = do
    conn <- connection
    let uvm = uvsManager conn
        handle = uvsHandle conn
    connSlot <- uvSlot uvm
    connReq <- uvReq uV_CONNECT
    liftIO $ do
        forM_ local $ \ local' -> withSockAddr local' $ \ localPtr ->
            uvTCPBind handle localPtr False

        withSockAddr target $ \ target' -> do
            m <- getBlockMVar uvm connSlot
            tryTakeMVar m
            pokeUVReqData connReq connSlot
            withUVManager' uvm $ uvTCPConnect connReq handle target'
            throwUVIfMinus_ $ takeMVar m
    return conn



{-
data ServerConfig = ServerConfig
    { serverEndPoint :: Either V.Bytes SockAddr
    , serverListeningThreadNum :: Int
    , serverWorker :: TCPConn -> IO ()
    , serverErrorHandler :: Exception -> IO ()
    }

data Server = Server

startServer :: ServerConfig -> IO Server

closeServer :: Server -> IO ()

--------------------------------------------------------------------------------

foreign import ccall unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO (E.WSAReturn CInt)
-}

