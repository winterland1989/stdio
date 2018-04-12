{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
  , initTCPConnection
  , ServerConfig(..)
  , startServer
  , module System.IO.Net.SockAddr
  ) where


import System.IO.Net.SockAddr
import System.IO.Exception
import System.IO.Buffered
import System.IO.UV.Manager
import System.IO.UV.Stream
import System.IO.UV.Internal
import Control.Concurrent.MVar
import Foreign.Ptr
import Foreign.C.Types (CInt(..))
import Data.Int
import Data.IORef.Unboxed
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Primitive.PrimArray
import Foreign.PrimArray


initTCPHandle :: HasCallStack => UVManager -> Resource (Ptr UVHandle)
initTCPHandle = initUVHandle uV_TCP (\ loop handle -> uv_tcp_init loop handle >> return handle)

initTCPStream :: HasCallStack => Resource UVStream
initTCPStream = do
    uvm    <- liftIO getUVManager
    rslot  <- initUVSlot uvm
    wslot  <- initUVSlot uvm
    handle <- initTCPHandle uvm
    req    <- initUVReq uV_WRITE
    liftIO $ do
        pokeUVHandleData handle rslot
        pokeUVReqData req wslot
        return (UVStream handle rslot req wslot uvm)


initTCPConnection :: HasCallStack
        => SockAddr
        -> Maybe SockAddr
        -> Resource UVStream
initTCPConnection target local = do
    conn <- initTCPStream
    let uvm = uvsManager conn
        handle = uvsHandle conn
    connSlot <- initUVSlot uvm
    connReq <- initUVReq uV_CONNECT
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



data ServerConfig = ServerConfig
    { serverAddr :: SockAddr
    , serverBackLog            :: Int
    , serverWorker :: UVStream -> IO ()
    , serverErrorHandler :: SomeIOException -> IO ()
    }

startServer :: ServerConfig -> IO ()
startServer ServerConfig{..} =
    withResource initTCPStream $ \ server ->
    withSockAddr serverAddr $ \ addrPtr -> do

        let serverHandle = uvsHandle server
            serverManager = uvsManager server

        uvTCPBind serverHandle addrPtr False
        uvDisableSimultaneousAccept serverHandle

        m <- getBlockMVar serverManager (uvsReadSlot server)
        tryTakeMVar m
        withUVManager' serverManager $ uvListen serverHandle (fromIntegral serverBackLog)

        forever $ do

            fd <- throwUVIfMinus $ takeMVar m

            forkBa . withResource initTCPStream $ \ client -> do
                withUVManager' (uvsManager client) $ do
                    uvTCPOpen (uvsHandle client) (fromIntegral fd)
                    uvTCPNodelay (uvsHandle client) True
                serverWorker client

--------------------------------------------------------------------------------

-- | Disable so called simultaneous accept, we can loop accept until EAGAIN in haskell
-- side instead of get multiple event in C side.
--
uvDisableSimultaneousAccept :: HasCallStack => Ptr UVHandle -> IO ()
uvDisableSimultaneousAccept handle = throwUVIfMinus_ (uv_tcp_simultaneous_accepts handle 0)
foreign import ccall unsafe uv_tcp_simultaneous_accepts  :: Ptr UVHandle -> CInt -> IO CInt

uvListen :: HasCallStack => Ptr UVHandle -> CInt -> IO ()
uvListen handle backlog = throwUVIfMinus_ (hs_uv_listen handle backlog)
foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt

uvFileno :: HasCallStack => Ptr UVHandle -> IO CInt
uvFileno = throwUVIfMinus . hs_uv_fileno
foreign import ccall unsafe hs_uv_fileno :: Ptr UVHandle -> IO CInt
