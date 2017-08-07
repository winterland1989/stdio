{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Address
  ( -- * name to address
    SocketAddress(..)
  , SockAddr
  , RawSockAddr 
  , newEmptyRawSockAddr
   -- * IP Address
  , SockAddrInet(..)
   -- ** IPv4 address
  , InetAddr
  , inetAny
  , inetBroadcast
  , inetNone
  , inetLoopback
  , inetUnspecificGroup
  , inetAllHostsGroup
  , inetMaxLocalGroup
  , inetAddrToTuple
  , tupleToInetAddr
   -- ** IPv6 address
  , Inet6Addr
  , inet6Any
  , inet6Loopback
  , inet6AddrToTuple
  , tupleToInet6Addr
  , FlowInfo
  , ScopeID
  -- * address to name
  , AddrInfo(..)
  , AddrInfoHint(..)
  , AddrInfoFlag
  , getAddrInfo
  -- * port numbber
  , PortNumber 
  , aNY_PORT
  , htons
  , ntohs
  -- * family, type, protocol
  , SocketFamily(..)
  , aF_UNSPEC
  , aF_INET
  , aF_INET6
  , SocketType(..)
  , sOCK_DGRAM
  , sOCK_STREAM
  , sOCK_SEQPACKET
  , sOCK_RAW
  , sOCK_RDM
  , SocketProtocol(..)
  , iPPROTO_TCP
  , iPPROTO_UDP
  ) where

import Foreign
import Foreign.C
import qualified System.IO.Socket.Exception as E
import GHC.Stack.Compat
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import qualified Data.List as List
import Data.Bits
import Numeric (showHex)
import Data.Typeable
import Data.Ratio
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.Vector as V
import Data.CBytes
import Foreign.Marshal.Utils (copyBytes)
import Network (withSocketsDo)
import Data.Primitive.PrimArray
import Control.Monad.Primitive

#include "HsNet.h" 

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

--------------------------------------------------------------------------------

-- | Type class to unify different `sockaddr` stuct.
--
class Show addr => SocketAddress addr where
    fromRawSockAddr :: RawSockAddr -> Maybe addr
    withSockAddr :: addr -> (Ptr SockAddr -> IO a) -> IO a
    sockAddrSize :: addr -> Int
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
newtype RawSockAddr = RawSockAddr (MutablePrimArray RealWorld Word8) -- TODO: better instance

newEmptyRawSockAddr :: IO RawSockAddr
newEmptyRawSockAddr = do
    mpa <- newAlignedPinnedPrimArray
        (#size struct sockaddr_storage)
        (#alignment struct sockaddr_storage)

    setPrimArray mpa 0 (#size struct sockaddr_storage) 0
    return (RawSockAddr mpa)

instance Show RawSockAddr where
    show _ = "RawSockAddr"

instance SocketAddress RawSockAddr where
    fromRawSockAddr = Just . id
    withSockAddr = withRawSockAddr
    sockAddrSize (RawSockAddr pa) = unsafeDupablePerformIO $ sizeofMutablePrimArray pa
    sockAddrFamily raw = unsafeDupablePerformIO . withSockAddr raw $ \ p -> do
        family <- (#peek struct sockaddr, sa_family) p :: IO CSaFamily
        return (SocketFamily $ fromIntegral family)

withRawSockAddr :: RawSockAddr -> (Ptr SockAddr -> IO a) -> IO a
withRawSockAddr (RawSockAddr pa) f =  withMutablePrimArrayContents pa $ \ ptr -> f (castPtr ptr)

        -- | Use `RawSockAddr` as a pointer.
--
-- | IPv4 or IPv6 socket address, i.e. the `sockaddr_in` or `sockaddr_in6` struct.
-- 
data SockAddrInet 
    = SockAddrInet
        {-# UNPACK #-} !PortNumber  -- sin_port  (network byte order)
        {-# UNPACK #-} !InetAddr    -- sin_addr  (ditto)
    | SockAddrInet6
        {-# UNPACK #-} !PortNumber  -- sin6_port (network byte order)
        {-# UNPACK #-} !FlowInfo    -- sin6_flowinfo (ditto)
        {-# UNPACK #-} !Inet6Addr   -- sin6_addr (ditto)
        {-# UNPACK #-} !ScopeID     -- sin6_scope_id (ditto)
  deriving (Show, Eq, Ord)

type FlowInfo = Word32
type ScopeID = Word32

-- | Independent of endianness. For example @127.0.0.1@ is stored as @(127, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inetAddrToTuple' and 'tupleToInetAddr'.
--
newtype InetAddr = InetAddr Word32 deriving (Eq, Ord)
instance Show InetAddr where
    showsPrec _ ia = 
        let (a,b,c,d) = inetAddrToTuple ia
        in ("InetAddr " ++) . shows a . ('.':)
                            . shows b . ('.':)
                            . shows c . ('.':)
                            . shows d

-- | @0.0.0.0@
inetAny             :: InetAddr
inetAny              = InetAddr 0

-- | @255.255.255.255@
inetBroadcast       :: InetAddr
inetBroadcast        = tupleToInetAddr (255,255,255,255)

-- | @255.255.255.255@
inetNone            :: InetAddr
inetNone             = tupleToInetAddr (255,255,255,255)

-- | @127.0.0.1@
inetLoopback        :: InetAddr
inetLoopback         = tupleToInetAddr (127,  0,  0,  1)

-- | @224.0.0.0@
inetUnspecificGroup :: InetAddr
inetUnspecificGroup  = tupleToInetAddr (224,  0,  0,  0)

-- | @224.0.0.1@
inetAllHostsGroup   :: InetAddr
inetAllHostsGroup    = tupleToInetAddr (224,  0,  0,  1)

-- | @224.0.0.255@
inetMaxLocalGroup   :: InetAddr
inetMaxLocalGroup    = tupleToInetAddr (224,  0,  0,255)

instance Storable InetAddr where
    sizeOf _ = sizeOf (undefined :: Word32)
    alignment _ = alignment (undefined :: Word32) 
    peek p = (InetAddr . ntohl) `fmap` peekByteOff p 0
    poke p (InetAddr ia) = pokeByteOff p 0 (htonl ia)

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(127, 0, 0, 1)@
-- regardless of host endianness.
inetAddrToTuple :: InetAddr -> (Word8, Word8, Word8, Word8)
inetAddrToTuple (InetAddr ia) =
    let byte i = fromIntegral (ia `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToInetAddr :: (Word8, Word8, Word8, Word8) -> InetAddr
tupleToInetAddr (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in InetAddr $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inet6AddrToTuple' and 'tupleToInet6Addr'.
--
data Inet6Addr = Inet6Addr {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32 deriving (Eq, Ord)

instance Show Inet6Addr where
    showsPrec _ i6a = 
        let (a,b,c,d,e,f,g,h) = inet6AddrToTuple i6a
        in ("Inet6Addr " ++) . showHex a . (':':)
                             . showHex b . (':':)
                             . showHex c . (':':)
                             . showHex d . (':':)
                             . showHex e . (':':)
                             . showHex f . (':':)
                             . showHex g . (':':)
                             . showHex h

-- | @::@
inet6Any      :: Inet6Addr
inet6Any       = Inet6Addr 0 0 0 0

-- | @::1@
inet6Loopback :: Inet6Addr
inet6Loopback  = Inet6Addr 0 0 0 1

inet6AddrToTuple :: Inet6Addr -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
inet6AddrToTuple (Inet6Addr w3 w2 w1 w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

tupleToInet6Addr :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> Inet6Addr
tupleToInet6Addr (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in  Inet6Addr (w7 `add` w6) (w5 `add` w4) (w3 `add` w2) (w1 `add` w0)

instance Storable Inet6Addr where
    sizeOf _    = #size struct in6_addr
    alignment _ = #alignment struct in6_addr

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ Inet6Addr a b c d

    poke p (Inet6Addr a b c d) = do
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

    sockAddrSize (SockAddrInet _ _) = #size struct sockaddr_in
    sockAddrSize (SockAddrInet6 _ _ _ _) = #size struct sockaddr_in6

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

-- | Use the @Num@ instance (i.e. use a literal or 'fromIntegral') to create a
-- @PortNumber@ value with the correct network-byte-ordering.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable)
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.

aNY_PORT :: PortNumber
aNY_PORT = 0

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (intToPortNumber x, y)) . readsPrec n

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

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

aiFlagMapping :: [(AddrInfoFlag, CInt)]
aiFlagMapping =
    [
#if HAVE_DECL_AI_ADDRCONFIG
     (AI_ADDRCONFIG, #const AI_ADDRCONFIG),
#else
     (AI_ADDRCONFIG, 0),
#endif
#if HAVE_DECL_AI_ALL
     (AI_ALL, #const AI_ALL),
#else
     (AI_ALL, 0),
#endif
     (AI_CANONNAME, #const AI_CANONNAME),
     (AI_NUMERICHOST, #const AI_NUMERICHOST),
#if HAVE_DECL_AI_NUMERICSERV
     (AI_NUMERICSERV, #const AI_NUMERICSERV),
#else
     (AI_NUMERICSERV, 0),
#endif
     (AI_PASSIVE, #const AI_PASSIVE),
#if HAVE_DECL_AI_V4MAPPED
     (AI_V4MAPPED, #const AI_V4MAPPED)
#else
     (AI_V4MAPPED, 0)
#endif
    ]

data AddrInfo = AddrInfo 
    { addrFamily :: SocketFamily
    , addrSocketType :: SocketType
    , addrProtocol :: SocketProtocol
    , addrAddressRaw :: RawSockAddr
    , addrCanonName :: Maybe CBytes
    } deriving (Show, Typeable)

data AddrInfoHint = AddrInfoHint 
    { addrHintFlags :: [AddrInfoFlag]
    , addrHintFamily :: SocketFamily
    , addrHintSocketType :: SocketType
    , addrHintProtocol :: SocketProtocol
    } deriving (Show, Eq)

peekAddrInfo :: Ptr AddrInfo -> IO AddrInfo
peekAddrInfo p = do
    ai_family <- (#peek struct addrinfo, ai_family) p
    ai_socktype <- (#peek struct addrinfo, ai_socktype) p
    ai_protocol <- (#peek struct addrinfo, ai_protocol) p

    addrlen <- (#peek struct addrinfo, ai_addrlen) p 
    ai_addr <- (#peek struct addrinfo, ai_addr) p >>= copySockAddr addrlen

    ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

    ai_canonname <- fromCString free ai_canonname_ptr

    return AddrInfo
        { addrFamily = ai_family
        , addrSocketType = ai_socktype
        , addrProtocol = ai_protocol
        , addrAddressRaw = ai_addr
        , addrCanonName = ai_canonname
        }
  where
    -- | Copy a 'SockAddr' from the given memory location.
    copySockAddr :: CInt -> Ptr SockAddr -> IO RawSockAddr
    copySockAddr len p = do
        mpa <- newAlignedPinnedPrimArray
                (fromIntegral len) (#alignment struct sockaddr_storage)
        copyMutablePrimArrayFromPtr mpa 0 (castPtr p) (fromIntegral len)
        return (RawSockAddr mpa)

pokeAddrInfoHint :: Ptr AddrInfo -> AddrInfoHint -> IO ()
pokeAddrInfoHint p (AddrInfoHint flags family socketType protocol) = do

        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p family
        (#poke struct addrinfo, ai_socktype) p socketType
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

withAddrInfoHint :: AddrInfoHint -> (Ptr AddrInfo -> IO a) -> IO a
withAddrInfoHint hint = 
    allocaBytesAligned (#size struct addrinfo) (#alignment struct addrinfo)

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b
packBits mapping xs = List.foldl' pack 0 mapping
    where pack acc (k, v) | k `elem` xs = acc .|. v
                          | otherwise   = acc

-- | Unpack a bitmask into a list of values.

unpackBits :: (Num b, Bits b) => [(a, b)] -> b -> [a]

-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

    
-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bind'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
--
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
-- >>> addrAddress addr
-- 127.0.0.1:80

getAddrInfo :: HasCallStack
            => Maybe AddrInfoHint -- ^ preferred socket type or protocol
            -> Maybe CBytes       -- ^ host name to look up
            -> Maybe CBytes       -- ^ service name to look up
            -> IO [AddrInfo]      -- ^ resolved addresses, with "best" first

getAddrInfo hints node service = withSocketsDo $
    maybeWith withCBytes node $ \ c_node ->
    maybeWith withCBytes service $ \ c_service ->
    maybeWith withAddrInfoHint hints $ \ c_hints ->
        alloca $ \ ptr_ptr_addrs -> do
          ret <- E.throwAddrErrorIfNonZero callStack
                    ("hint:" ++ show hints ++
                    ",host:" ++ show node ++ 
                    ",service:" ++ show service)
                    (c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs)
          ptr_addrs <- peek ptr_ptr_addrs
          ais <- followAddrInfo ptr_addrs
          c_freeaddrinfo ptr_addrs          -- don't forget free them
          return ais
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
  where
#if defined(darwin_HOST_OS)
    filteredHints = case service of
        Nothing -> fmap (\ h -> h { addrHintFlags = delete AI_NUMERICSERV (addrHintFlags h) }) hints
        _       -> hints
#else
    filteredHints = hints
#endif
    followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]
    followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                          | otherwise = do
        a <- peekAddrInfo ptr_ai
        as <- (#peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
        return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo) -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

--------------------------------------------------------------------------------

newtype SocketFamily = SocketFamily CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketType = SocketType CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketProtocol = SocketProtocol CInt deriving (Show, Read, Eq, Ord, Typeable)

instance Storable SocketFamily where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketFamily `fmap` peek (castPtr ptr)             
    poke ptr (SocketFamily v) = poke (castPtr ptr) v

instance Storable SocketType where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketType `fmap` peek (castPtr ptr)             
    poke ptr (SocketType v) = poke (castPtr ptr) v

instance Storable SocketProtocol where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketProtocol `fmap` peek (castPtr ptr)             
    poke ptr (SocketProtocol v) = poke (castPtr ptr) v

--------------------------------------------------------------------------------

-- | unspecified
aF_UNSPEC           = SocketFamily (#const AF_UNSPEC          )
-- | internetwork: UDP, TCP, etc
aF_INET             = SocketFamily (#const AF_INET            )
-- | Internet Protocol version 6
aF_INET6            = SocketFamily (#const AF_INET6           )

--------------------------------------------------------------------------------

#ifdef SOCK_STREAM
sOCK_STREAM = SocketType (#const SOCK_STREAM)
#endif
#ifdef SOCK_DGRAM
sOCK_DGRAM = SocketType (#const SOCK_DGRAM) 
#endif
#ifdef SOCK_RAW
sOCK_RAW = SocketType (#const SOCK_RAW) 
#endif
#ifdef SOCK_RDM
sOCK_RDM = SocketType (#const SOCK_RDM) 
#endif
#ifdef SOCK_SEQPACKET
sOCK_SEQPACKET = SocketType (#const SOCK_SEQPACKET) 
#endif

--------------------------------------------------------------------------------

iPPROTO_IP :: SocketProtocol
iPPROTO_IP = SocketProtocol (#const IPPROTO_IP)

iPPROTO_TCP :: SocketProtocol
iPPROTO_TCP = SocketProtocol (#const IPPROTO_TCP)

iPPROTO_UDP :: SocketProtocol
iPPROTO_UDP = SocketProtocol (#const IPPROTO_UDP)
