{-# LANGUAGE CPP #-}

module System.IO.Socket.TCP where

import qualified System.IO.Exception as E
import qualified System.IO.Socket.Exception as E
import System.IO.Socket.Address
import System.IO.Socket.Base
import Foreign.C
import Foreign hiding (void)
import GHC.Stack.Compat
import Network.Socket (withSocketsDo)
import System.IO.UV.Manager
import System.IO.UV.Base
import Control.Concurrent
import System.IO.Handle
import Data.IORef
import Data.Array
import Control.Monad
import System.Posix.Types (CSsize(..))

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
        fromIntegral `fmap` E.retryInterruptWaitBlock dev
#if defined(mingw32_HOST_OS)
                (fromIntegral `fmap` c_recv fd buf (fromIntegral bufSiz) 0 :: IO (E.WSAReturn CSsize))
#else
                (c_recv fd buf (fromIntegral bufSiz) 0)
#endif
            (do pokeBufferTable uvm slot buf bufSiz
                E.throwIfError dev $ do
                     withUVManagerEnsureRunning uvm (hs_read_start handle)
                takeMVar =<< getBlockMVar uvm slot
                getResult uvm slot)


instance Output TCP where
    writeOutput tcp@(TCP handle _ _ reqLock slot uvm fd) buf bufSiz = withMVar reqLock $ \ req ->
        loop req buf bufSiz
      where
        loop req buf bufSiz = do
            let dev = show tcp
            r <- E.retryInterruptWaitBlock dev
#if defined(mingw32_HOST_OS)
                (fromIntegral `fmap` c_send fd buf (fromIntegral bufSiz) 0 :: IO (E.WSAReturn CSsize))
#else
                (c_send fd buf (fromIntegral bufSiz) 0)
#endif
                (do pokeBufferTable uvm slot buf bufSiz
                    E.throwIfError dev $
                        withUVManagerEnsureRunning uvm (hs_write req handle)
                    takeMVar =<< getBlockMVar uvm slot
                    r <- getResult uvm slot
                    if r >= 0
                    then return (fromIntegral bufSiz)
                    else return r)

            let r' = fromIntegral r
            when (r' < bufSiz)  $
                loop req (buf `plusPtr` r') (bufSiz - r')

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

setTCPNodelay :: TCP -> Bool -> IO ()
setTCPNodelay tcp@(TCP handle _ _ _ _ _ _) n =
    void . E.throwIfError (show tcp) $
        uv_tcp_nodelay handle (if n then 1 else 0)

data TCPListener addr = TCPListener CInt addr (IO ())

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
    let dev = show addr
    withSockAddr addr $ \ addrPtr ->
        with (fromIntegral $ sockAddrSize addr) $ \ lenPtr -> do
            SocketFd `fmap` E.retryInterruptWaitBlock dev
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
