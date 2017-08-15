{-# LANGUAGE CPP #-}

module System.IO.Socket.Base where

import qualified System.IO.Exception as E
import qualified System.IO.Socket.Exception as E
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
import System.Posix.Types (CSsize(..))

--------------------------------------------------------------------------------

newtype SocketFd = SocketFd CInt deriving (Show, Eq, Ord)
data BoundSocket addr = BoundSocket CInt addr deriving Show

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
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.WSAReturn CInt)

foreign import CALLCONV unsafe "HsNet.h send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.WSAReturn CInt)
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
    c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.UnixReturn CSsize)

foreign import CALLCONV unsafe "HsNet.h send"
    c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO (E.UnixReturn CSsize)
#endif
