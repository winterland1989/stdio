{-# LANGUAGE CPP #-}

module System.IO.Socket.Base where

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
import Control.Monad


data TCP = TCP
    { tcpUVHandle     :: Ptr UVHandle
    , tcpUVHandleSlot :: Int
    , tcpUVReadLock   :: MVar ()
    , tcpUVReqLock    :: MVar (Ptr UVReq) -- the write request and write lock
    , tcpUVReqSlot    :: Int
    , tcpUVManager    :: UVManager
    , tcpFd           :: CInt
    }

instance Show TCP where
    show tcp = "tcp" -- TODO: make it detailed

closeTCP :: TCP -> IO ()
closeTCP = undefined

instance Input TCP where
    readInput tcp@(TCP handle slot readLock _ _ uvm fd) buf bufSiz = withMVar readLock $ \ _ -> do
        let dev = show tcp
        (bufTable, bufSizTable) <- getBufferTable uvm

        pokeElemOff bufTable slot buf
        pokeElemOff bufSizTable slot (fromIntegral bufSiz)

        E.throwIfError dev $
            withUVManagerEnsureRunning uvm (hs_read_start handle)

        takeMVar =<< getBlockMVar uvm slot

        r <- E.throwIfError dev $ getResult uvm slot
        return (fromIntegral r)

instance Output TCP where
    writeOutput tcp@(TCP handle _ _ reqLock slot uvm fd) buf bufSiz = withMVar reqLock $ \ req ->  do
        let dev = show tcp

        (bufTable, bufSizTable) <- getBufferTable uvm

        pokeElemOff bufTable slot buf
        pokeElemOff bufSizTable slot (fromIntegral bufSiz)

        E.throwIfError (show tcp) $
            withUVManagerEnsureRunning uvm (hs_write req handle)

        takeMVar =<< getBlockMVar uvm slot

        _ <- E.throwIfError dev $ getResult uvm slot
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
    fd <- E.throwIfError dev $
                c_socket family typ proto
    withSocketsDo $ return (SocketFd fd)   -- TODO: set non blocking here
  where
    dev = show sfamily ++ ", " ++ show stype ++ ", " ++ show sproto


newTCP :: SocketFd -> IO TCP
newTCP (SocketFd fd) = do
    uvm <- getUVManager
    slotR <- allocSlot uvm
    slotW <- allocSlot uvm

    let dev = "tcp FD:" ++ show fd

    handle <- E.throwOOMIfNull dev $ hs_handle_init uV_TCP
    req <- E.throwOOMIfNull dev $ hs_req_init uV_WRITE

    withUVManager uvm $ \ loop -> do
        E.throwIfError dev $ uv_tcp_init loop handle
        E.throwIfError dev $ uv_tcp_open handle fd

    poke_uv_handle_data handle (fromIntegral slotR)
    poke_uv_req_data req (fromIntegral slotW)

    readLock <- newMVar ()
    reqLock <- newMVar req

    return (TCP handle slotR readLock reqLock slotW uvm fd)

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
        E.throwIfError (show s ++ ", " ++ show addr) $
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
    let dev = show s

    uvm <- getUVManager
    slot <- allocSlot uvm

    handle <- E.throwOOMIfNull dev $ hs_handle_init uV_POLL

    E.throwIfError dev $
        withUVManager uvm $ \ loop -> uv_poll_init_socket loop handle sock

    poke_uv_handle_data handle (fromIntegral slot)

    E.throwIfError dev $ c_listen sock (fromIntegral backlog)

    let wait = do
            E.throwIfError dev $
                withUVManagerEnsureRunning uvm (hs_poll_start handle uV_READABLE)
            takeMVar =<< getBlockMVar uvm slot

    return (TCPListener sock addr wait)


-- | Accept a connection with 'TCPListener'.
--
--
accept :: HasCallStack
       => TCPListener addr           -- queue socket
       -> IO SocketFd
accept s@(TCPListener sock addr wait) = do
    addr <- newEmptyRawSockAddr
    withSockAddr addr $ \ addrPtr ->
        with (fromIntegral $ sockAddrSize addr) $ \ lenPtr -> do
            SocketFd `fmap` E.retryInterruptWaitBlock (show addr)
                (c_accept sock addrPtr lenPtr)
                (wait >> c_accept sock addrPtr lenPtr)


#if defined(SO_REUSEPORT) && defined(linux_HOST_OS)
-- This function will start event based accept loop on up to N captibilities, automatically start worker
-- thread using given callback and distrubite them on to all captibilities.
--
-- This function leverage the fact that multiple event loop can improve accepting rate but this will suffer
-- from multiple wakeups, aka. @thundering herd@ problem. So it's recommand to limit the N parameter.
#if defined(SO_REUSEPORT) && defined(linux_HOST_OS)
-- On system support @SO_REUSEPORT@ and kernel load balance(e.g. linux > 3.9), please use 'multiAccept' to
-- avoid this problem without losing multithread accept.
--
#endif
multiAccept :: [TCPListener addr]
multiAccept = undefined
#endif

--------------------------------------------------------------------------------

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#define CALLCONV stdcall
#else
#define CALLCONV ccall
#endif
#if defined(mingw32_HOST_OS)

foreign import CALLCONV unsafe "HsNet.h socket"
    c_socket :: CInt -> CInt -> CInt -> IO (E.WSAReturn CInt)

foreign import CALLCONV unsafe "HsNet.h bind"
    c_bind :: CInt -> Ptr SockAddr -> CInt -> IO (E.WSAReturn CInt)

foreign import CALLCONV unsafe "HsNet.h listen"
    c_listen :: CInt -> CInt -> IO (E.WSAReturn CInt)

foreign import CALLCONV unsafe "HsNet.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO (E.WSAReturn CInt)

foreign import CALLCONV unsafe "HsNet.h recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.WSAReturn CSize)

foreign import CALLCONV unsafe "HsNet.h send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.WSAReturn CSize)
#else
foreign import CALLCONV unsafe "HsNet.h socket"
    c_socket :: CInt -> CInt -> CInt -> IO (E.UnixReturn CInt)

foreign import CALLCONV unsafe "HsNet.h bind"
    c_bind :: CInt -> Ptr SockAddr -> CInt -> IO (E.UnixReturn CInt)

foreign import CALLCONV unsafe "HsNet.h listen"
    c_listen :: CInt -> CInt -> IO (E.UnixReturn CInt)

foreign import CALLCONV unsafe "HsNet.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO (E.UnixReturn CInt)

foreign import CALLCONV unsafe "HsNet.h recv"
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.UnixReturn CSize)

foreign import CALLCONV unsafe "HsNet.h send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.UnixReturn CSize)
#endif
