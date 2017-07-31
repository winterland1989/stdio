{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Address
 ( -- * name to address
   SockAddr(..)
 , isSupportedSockAddr
 , HostAddress
 , hostAddressToTuple
 , tupleToHostAddress
 , HostAddress6
 , hostAddress6ToTuple
 , tupleToHostAddress6
 , FlowInfo
 , ScopeID
 , PortNumber
 -- * address to name
 , HostName
 , ServiceName
 , AddrInfo(..)
 , AddrInfoFlag
 , addrInfoFlagImplemented
 , defaultHints
 , getAddrInfo
 , NameInfoFlag
 , getNameInfo
 -- * port numbber
 , PortNumber 
 , htonl
 , ntohl
 ) where

import System.IO.Socket.Types
import Foreign
import Foreign.C
import qualified System.IO.Exception as E
import GHC.Stack.Compat
import qualified Data.List as List
import Data.Bits
import Data.Typeable

#include "HsNet.h" 

--------------------------------------------------------------------------------

data SockAddr

class SocketAddress addr where
    fromSockAddrPtr :: SockAddrInfo -> IO (Maybe addr)
    toSockAddrPtr :: addr -> IO (ForeignPtr SockAddr)
    -- | Computes the storage requirements (in bytes) of the given type of 'SockAddr'. 
    -- For general case like @ForeignPtr SockAddr@, size of @sockaddr_storage@ is used.
    sizeOfSockAddr :: addr -> Int

instance SocketAddress (ForeignPtr SockAddr) where
    fromSockAddr = id
    toSockAddr = id
    sizeOfSockAddr (SockAddrInet _ _) = #const sizeof(struct sockaddr_storage)

data SockAddrInet = SockAddrInet
        PortNumber  -- sin_port  (network byte order)
        HostAddress -- sin_addr  (ditto)

instance SocketAddress SockAddrInet where
    fromSockAddr = 

  family <- (#peek struct sockaddr, sa_family) p
  case family :: CSaFamily of
    (#const AF_INET) -> do
        addr <- (#peek struct sockaddr_in, sin_addr) p
        port <- (#peek struct sockaddr_in, sin_port) p
        return (SockAddrInet (PortNum port) addr)
    toSockAddr = id
    sizeOfSockAddr _ = #const sizeof(struct sockaddr_in)

#if defined(IPV6_SOCKET_SUPPORT)
data SockAddrInet6 = SockAddrInet6
        PortNumber      -- sin6_port (network byte order)
        FlowInfo        -- sin6_flowinfo (ditto)
        HostAddress6    -- sin6_addr (ditto)
        ScopeID         -- sin6_scope_id (ditto)
instance SocketAddress addr where


    sizeOfSockAddr _ = #const sizeof(struct sockaddr_in6)
#endif
--------------------------------------------------------------------------------

data AddrInfo = AddrInfo 
    { addrFamily :: SocketFamily
    , addrSocketType :: SocketType
    , addrProtocol :: SocketProtocol
    , addrAddressRaw :: ForeignPtr SockAddr
    , addrCanonName :: Maybe String
    } deriving (Eq, Show, Typeable)

data AddrInfoHint = AddrInfoHint 
    { addrHintFlags :: [AddrInfoFlag]
    , addrHintFamily :: Maybe SocketFamily
    , addrHintSocketType :: SocketType
    , addrHintProtocol :: SocketProtocol
    } deriving (Show, Eq)

