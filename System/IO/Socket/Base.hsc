module System.IO.Socket.Base where

import qualified System.IO.Socket.Exception as E
import qualified System.IO.UV.Exception as E
import qualified System.IO.Exception as E
import System.IO.Socket.Address
import Foreign.C
import Foreign
import GHC.Stack.Compat
import Network.Socket (withSocketsDo)
import System.IO.UV.Manager
import System.IO.UV.Base
import Control.Concurrent
import System.IO.Handle
import Data.IORef
import Data.Array

#include "HsNet.h"

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif


data TCP = TCP
    { tcpHandle :: Ptr UVHandle
    , tcpSlot   :: Int
    , tcpManager :: UVManager
    }

instance Show TCP where
    show tcp = "tcp" -- TODO: make it detailed

closeTCP :: TCP -> IO ()
closeTCP = undefined

instance Input TCP where
    readInput tcp@(TCP uvhandle slot uvm) buf bufSiz = do
        ensureUVMangerRunning uvm
        (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
        resultTable <- peekResultTable (uvmLoopData uvm)
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do

            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            pokeElemOff resultTable slot 0
            E.throwUVErrorIfMinus callStack  (show tcp) $ hs_read_start uvhandle
        btable <- readIORef $ uvmBlockTableR uvm
        takeMVar (indexArr btable slot)
        fromIntegral `fmap` peekElemOff resultTable slot

instance Output TCP where
    writeOutput tcp@(TCP uvhandle slot uvm) buf bufSiz = do
        ensureUVMangerRunning uvm
        (bufTable, bufSizTable) <- peekWriteBuffer (uvmLoopData uvm)
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            E.throwUVErrorIfMinus callStack  (show tcp) $ hs_read_start uvhandle
        btable <- readIORef $ uvmBlockTableW uvm
        takeMVar (indexArr btable slot)
        return ()

--------------------------------------------------------------------------------

newtype SocketFd = SocketFd CInt deriving (Show, Eq, Ord)
data BoundSocket addr = BoundSocket CInt addr deriving Show
data TCPListener addr = TCPListener CInt addr (IO ())

socket :: HasCallStack
       => SocketFamily      -- Family Name (usually AF_INET)
       -> SocketType        -- Socket Type (usually sOCK_STREAM)
       -> SocketProtocol    -- Protocol Number (getProtocolByName to find value)
       -> IO SocketFd       -- Unconnected Socket
socket sfamily@(SocketFamily family) stype@(SocketType typ) sproto@(SocketProtocol proto) = do
    fd <- E.throwSocketErrorIfMinus1Retry callStack dev $
                c_socket family typ proto
    withSocketsDo $ return (SocketFd fd)   -- TODO: set non blocking here
  where
    dev = show sfamily ++ ", " ++ show stype ++ ", " ++ show sproto


newTCP :: SocketFd -> IO TCP
newTCP (SocketFd fd) = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
        dev = "tcp FD:" ++ show fd
    p <- E.throwOOMIfNull callStack dev $ hs_handle_init uV_TCP

    withMVar (uvmFreeSlotList uvm) $ \ _ -> do
        E.throwUVErrorIfMinus callStack dev $ uv_tcp_init loop p

    E.throwUVErrorIfMinus callStack dev $ uv_tcp_open p fd

    poke_uv_handle_data p (fromIntegral slot)
    return (TCP p slot uvm)

-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'aNY_PORT' is passed then the system assigns the next available
-- use port.
--
bind :: (HasCallStack, SocketAddress addr)
     => SocketFd  -- Unconnected Socket
     -> addr      -- Address to Bind to
     -> IO (BoundSocket addr)
bind s@(SocketFd sock) addr = do
    let siz = sockAddrSize addr
    withSockAddr addr $ \ p -> do
        E.throwSocketErrorIfMinus1Retry_ callStack (show s ++ ", " ++ show addr) $
            c_bind sock p (fromIntegral siz)
    return (BoundSocket sock addr)


-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: (HasCallStack, SocketAddress addr)
       => BoundSocket addr  -- Connected & Bound Socket
       -> Int               -- Queue Length
       -> IO (TCPListener addr)
listen s@(BoundSocket sock addr) backlog = do
    uvm <- getUVManager
    ensureUVMangerRunning uvm
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
        dev = show s
    handle <- E.throwOOMIfNull callStack dev $ hs_handle_init uV_POLL

    withMVar (uvmFreeSlotList uvm) $ \ _ -> do
        E.throwUVErrorIfMinus callStack dev $ uv_poll_init_socket loop handle sock

    poke_uv_handle_data handle (fromIntegral slot)

    c_listen sock (fromIntegral backlog)

    btable <- readIORef $ uvmBlockTableR uvm

    let wait = do
        E.throwUVErrorIfMinus callStack dev $ hs_poll_start handle (getUVPollEvent uV_READABLE)
        takeMVar $ indexArr btable slot


    return (TCPListener sock addr wait)


-- | Accept a connection with 'TCPListener'.
--
-- This function will start event based accept loop on up to N captibilities, automatically start worker
-- thread using given callback and distrubite them on to all captibilities.
--
-- This function leverage the fact that multiple event loop can improve accepting rate but this will suffer 
-- from multiple wakeups, aka. @thundering herd@ problem. So it's recommand to limit the N parameter.
#if defined(SO_REUSEPORT) && defined(linux_HOST_OS)
-- On system support @SO_REUSEPORT@ and kernel load balance(e.g. linux > 3.9), please use 'multiAccept' to
-- avoid this problem without losing multithread accept.
#endif
--
accept :: HasCallStack
       => TCPListener addr           -- queue socket
       -> IO SocketFd
accept s@(TCPListener sock addr wait) = do
    addr <- newEmptyRawSockAddr
    withSockAddr addr $ \ addrPtr ->
        with (fromIntegral $ sockAddrSize addr) $ \ lenPtr -> do
            SocketFd `fmap` E.throwErrnoIfMinus1RetryMayBlock callStack (show addr)
                (c_accept sock addrPtr lenPtr) wait

#if defined(SO_REUSEPORT) && defined(linux_HOST_OS)
multiAccept :: [TCPListener addr]
multiAccept = undefined
#endif


--------------------------------------------------------------------------------

foreign import #{CALLCONV} unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import #{CALLCONV} unsafe "bind"
    c_bind :: CInt -> Ptr SockAddr -> CInt -> IO CInt

foreign import #{CALLCONV} unsafe "listen"
    c_listen :: CInt -> CInt -> IO CInt

foreign import #{CALLCONV} unsafe "accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
