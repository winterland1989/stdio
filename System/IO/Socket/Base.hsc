module System.IO.Socket.Base where

import qualified System.IO.Socket.Exception as E
import System.IO.Socket.Address
import Foreign.C
import Foreign
import GHC.Stack.Compat
import Network.Socket (withSocketsDo)

#include "HsNet.h"

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

newtype SocketFd = SocketFd CInt deriving (Show, Eq, Ord)

socket :: HasCallStack
       => SocketFamily      -- Family Name (usually AF_INET)
       -> SocketType        -- Socket Type (usually Stream)
       -> SocketProtocol    -- Protocol Number (getProtocolByName to find value)
       -> IO SocketFd       -- Unconnected Socket
socket sfamily@(SocketFamily family) stype@(SocketType typ) sproto@(SocketProtocol proto) = do
    fd <- E.throwSocketErrorIfMinus1Retry callStack dev $
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
     -> IO ()
bind (SocketFd sock) addr = do
    let siz = sockAddrSize addr
    withSockAddr addr $ \ p -> do
        E.throwSocketErrorIfMinus1Retry callStack (show addr) $
            c_bind sock p (fromIntegral siz)
    return ()

--------------------------------------------------------------------------------

foreign import #{CALLCONV} unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import #{CALLCONV} unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt -> IO CInt

