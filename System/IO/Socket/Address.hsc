{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Address
  ( -- * name to address
    SocketAddress(..)
  , RawSockAddr 
  , SockAddrInet(..)
  , HostAddress
  , hostAddressToTuple
  , tupleToHostAddress
  , HostAddress6
  , hostAddress6ToTuple
  , tupleToHostAddress6
  , FlowInfo
  , ScopeID
  -- * address to name
  , AddrInfo(..)
  , AddrInfoFlag
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
import Data.Ratio
import System.IO.Unsafe (unsafeDupablePerformIO)

#include "HsNet.h" 

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

--------------------------------------------------------------------------------

-- | Type class to unify different `sockaddr` stuct.
--
class SocketAddress addr where
    fromRawSockAddr :: RawSockAddr -> Maybe addr
    withSockAddr :: addr -> (Ptr SockAddr -> IO a) -> IO a
    sockAddrFamily :: addr -> SocketFamily

#if defined(WITH_WINSOCK)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_HOST_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- | A type tag for `sockaddr` pointers.
data SockAddr

-- | A ghc heap copy of OS @sockaddr@ struct, you can use it directly without parsing
-- if you don't care about or don't know the structure of @sockaddr@.
--
newtype RawSockAddr = RawSockAddr (ForeignPtr SockAddr) deriving (Show, Eq, Ord)

instance SocketAddress RawSockAddr where
    fromRawSockAddr = Just . id
    withSockAddr = withRawSockAddr
    sockAddrFamily raw = unsafeDupablePerformIO . withSockAddr raw $ \ p -> do
        family <- (#peek struct sockaddr, sa_family) p :: IO CSaFamily
        return (SocketFamily $ fromIntegral family)

withRawSockAddr :: RawSockAddr -> (Ptr SockAddr -> IO a) -> IO a
withRawSockAddr (RawSockAddr fptr) f =  withForeignPtr fptr $ \ ptr -> f ptr

        -- | Use `RawSockAddr` as a pointer.
--
-- | IPv4 or IPv6 socket address, i.e. the `sockaddr_in` or `sockaddr_in6` struct.
-- 
data SockAddrInet 
    = SockAddrInet
        PortNumber  -- sin_port  (network byte order)
        HostAddress -- sin_addr  (ditto)
    | SockAddrInet6
        PortNumber      -- sin6_port (network byte order)
        FlowInfo        -- sin6_flowinfo (ditto)
        HostAddress6    -- sin6_addr (ditto)
        ScopeID         -- sin6_scope_id (ditto)

type FlowInfo = Word32
type ScopeID = Word32

-- | The raw network byte order number is read using host byte order.
-- Therefore on little-endian architectures the byte order is swapped. For
-- example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
-- and as @0x7f000001@ on big-endian hosts.
--
-- For direct manipulation prefer 'hostAddressToTuple' and 'tupleToHostAddress'.
--
type HostAddress = Word32

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'hostAddress6ToTuple' and 'tupleToHostAddress6'.
--
data HostAddress6 = HostAddress6 {-# UNPACK #-}!Word32
                                 {-# UNPACK #-}!Word32
                                 {-# UNPACK #-}!Word32
                                 {-# UNPACK #-}!Word32 deriving (Show, Eq, Ord)

hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (HostAddress6 w3 w2 w1 w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in  HostAddress6 (w7 `add` w6) (w5 `add` w4) (w3 `add` w2) (w1 `add` w0)

instance Storable HostAddress6 where
    sizeOf _    = #const sizeof(struct in6_addr)
    alignment _ = #alignment struct in6_addr

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ HostAddress6 a b c d

    poke p (HostAddress6 a b c d) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

instance SocketAddress SockAddrInet where
    fromRawSockAddr raw = unsafeDupablePerformIO . withRawSockAddr raw $ \ p -> do
        family <- (#peek struct sockaddr, sa_family) p
        case family :: CSaFamily of
            (#const AF_INET) -> do
                addr <- (#peek struct sockaddr_in, sin_addr) p
                port <- (#peek struct sockaddr_in, sin_port) p
                return (Just (SockAddrInet (PortNum port) addr))
            (#const AF_INET6) -> do
                port <- (#peek struct sockaddr_in6, sin6_port) p
                flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
                addr <- (#peek struct sockaddr_in6, sin6_addr) p
                scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
                return (Just (SockAddrInet6 (PortNum port) flow addr scope))
            _ -> return Nothing
    withSockAddr sa@(SockAddrInet (PortNum port) addr) f = do
        allocaBytesAligned (#size struct sockaddr_in) (#alignment struct sockaddr_in) $ \ p -> do
#if defined(darwin_HOST_OS)
            zeroMemory p (#const sizeof(struct sockaddr_in))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
            (#poke struct sockaddr_in, sin_len) p ((#const sizeof(struct sockaddr_in)) :: Word8)
#endif
            (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
            (#poke struct sockaddr_in, sin_port) p port
            (#poke struct sockaddr_in, sin_addr) p addr
            f p
    withSockAddr sa@(SockAddrInet6 (PortNum port) flow addr scope) f = do
        allocaBytesAligned (#size struct sockaddr_in) (#alignment struct sockaddr_in) $ \ p -> do
#if defined(darwin_HOST_OS)
            zeroMemory p (#const sizeof(struct sockaddr_in6))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
            (#poke struct sockaddr_in6, sin6_len) p ((#const sizeof(struct sockaddr_in6)) :: Word8)
#endif
            (#poke struct sockaddr_in6, sin6_family) p ((#const AF_INET6) :: CSaFamily)
            (#poke struct sockaddr_in6, sin6_port) p port
            (#poke struct sockaddr_in6, sin6_flowinfo) p flow
            (#poke struct sockaddr_in6, sin6_addr) p (addr)
            (#poke struct sockaddr_in6, sin6_scope_id) p scope
            f p

    sockAddrFamily (SockAddrInet _ _) = SocketFamily (#const AF_INET)
    sockAddrFamily (SockAddrInet6 _ _ _ _) = SocketFamily (#const AF_INET6)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = (#offset struct in6_addr, s6_addr)

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

--------------------------------------------------------------------------------

-- Port Numbers

-- | Use the @Num@ instance (i.e. use a literal) to create a
-- @PortNumber@ value with the correct network-byte-ordering. You
-- should not use the PortNum constructor. It will be removed in the
-- next release.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable)
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (intToPortNumber x, y)) . readsPrec n

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

#ifdef CALLCONV
#undef CALLCONV
#endif
#ifdef mingw32_HOST_OS
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

foreign import #{CALLCONV} unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "htons" htons :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import #{CALLCONV} unsafe "htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
                  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `fmap` peek (castPtr p)

--------------------------------------------------------------------------------

-- | Flags that control the querying behaviour of 'getAddrInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-25>
data AddrInfoFlag =
    -- | The list of returned 'AddrInfo' values will
    --   only contain IPv4 addresses if the local system has at least
    --   one IPv4 interface configured, and likewise for IPv6.
    --   (Only some platforms support this.)
      AI_ADDRCONFIG
    -- | If 'AI_ALL' is specified, return all matching IPv6 and
    --   IPv4 addresses.  Otherwise, this flag has no effect.
    --   (Only some platforms support this.)
    | AI_ALL
    -- | The 'addrCanonName' field of the first returned
    --   'AddrInfo' will contain the "canonical name" of the host.
    | AI_CANONNAME
    -- | The 'HostName' argument /must/ be a numeric
    --   address in string form, and network name lookups will not be
    --   attempted.
    | AI_NUMERICHOST
    -- | The 'ServiceName' argument /must/ be a port
    --   number in string form, and service name lookups will not be
    --   attempted. (Only some platforms support this.)
    | AI_NUMERICSERV
    -- | If no 'HostName' value is provided, the network
    --   address in each 'SockAddr'
    --   will be left as a "wild card", i.e. as either 'iNADDR_ANY'
    --   or 'iN6ADDR_ANY'.  This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show, Typeable)

data AddrInfo = AddrInfo 
    { addrFamily :: SocketFamily
    , addrSocketType :: SocketType
    , addrProtocol :: SocketProtocol
    , addrAddressRaw :: RawSockAddr
    , addrCanonName :: Maybe String
    } deriving (Eq, Show, Typeable)

data AddrInfoHint = AddrInfoHint 
    { addrHintFlags :: [AddrInfoFlag]
    , addrHintFamily :: Maybe SocketFamily
    , addrHintSocketType :: SocketType
    , addrHintProtocol :: SocketProtocol
    } deriving (Show, Eq)

