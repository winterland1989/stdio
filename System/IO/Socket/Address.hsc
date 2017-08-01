{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Address
  ( -- * name to address
    SocketAddress(..)
  , RawSockAddr 
  , SockAddrInet(..)
  , InetAddr
  , inetAddrToTuple
  , tupleToInetAddr
  , Inet6Addr
  , inet6AddrToTuple
  , tupleToInet6Addr
  , FlowInfo
  , ScopeID
  -- * address to name
  , AddrInfo(..)
  , AddrInfoFlag
  -- * port numbber
  , PortNumber 
  , htons
  , ntohs
  ) where

import Foreign
import Foreign.C
import qualified System.IO.Exception as E
import GHC.Stack.Compat
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import qualified Data.List as List
import Data.Bits
import Numeric (showHex)
import Data.Typeable
import Data.Ratio
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.Vector as V
import Foreign.PrimVector
import Foreign.Marshal.Utils (copyBytes)
import Network (withSocketsDo)

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

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
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
    , addrCanonName :: Maybe V.Bytes
    } deriving (Eq, Show, Typeable)

data AddrInfoHint = AddrInfoHint 
    { addrHintFlags :: [AddrInfoFlag]
    , addrHintFamily :: Maybe SocketFamily
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

    ai_canonname <- if ai_canonname_ptr == nullPtr
                    then return Nothing
                    else Just `fmap` peekCString ai_canonname_ptr

    socktype <- (#peek struct addrinfo, ai_socktype) p 
    return AddrInfo
        { addrFamily = ai_family
        , addrSocketType = socktype
        , addrProtocol = ai_protocol
        , addrAddressRaw = ai_addr
        , addrCanonName = ai_canonname
        }
  where
    -- | Copy a 'SockAddr' from the given memory location.
    copySockAddr :: CInt -> Ptr SockAddr -> IO RawSockAddr
    copySockAddr len p = do
        fp <- mallocPlainForeignPtrAlignedBytes len (#alignment struct sockaddr_storage)
        withForeignPtr fp $ \ p' ->
            copyBytes p' p len
        return (RawSockAddr fp)
        

pokeAddrInfoHint :: Ptr AddrInfo -> AddrInfoHint -> IO ()
pokeAddrInfoHint p (AddrInfoHint flags family socketType protocol) = do
        c_stype <- packSocketTypeOrThrow "AddrInfo.poke" socketType

        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p (packFamily family)
        (#poke struct addrinfo, ai_socktype) p c_stype
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

    
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

getAddrInfo :: Maybe AddrInfoHint -- ^ preferred socket type or protocol
            -> Maybe V.Bytes        -- ^ host name to look up
            -> Maybe V.Bytes        -- ^ service name to look up
            -> IO [AddrInfo]      -- ^ resolved addresses, with "best" first

getAddrInfo hints node service = withSocketsDo $
    maybeWith withPrimVector node $ \ c_node ->
    maybeWith withPrimVector service $ \c_service ->
    maybeWith with filteredHints $ \c_hints ->
        alloca $ \ ptr_ptr_addrs -> do
          ret <- throwAddrErrorIfNonZero (c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs)
          ptr_addrs <- peek ptr_ptr_addrs
          ais <- followAddrInfo ptr_addrs
          c_freeaddrinfo ptr_addrs
          return ais
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
  where
#if defined(darwin_HOST_OS)
    filteredHints = case service of
        Nothing -> fmap (\ h -> h { addrFlags = delete AI_NUMERICSERV (addrFlags h) }) hints
        _       -> hints
#else
    filteredHints = hints
#endif


followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]

followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                      | otherwise = do
    a <- peek ptr_ai
    as <- (#peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
    return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: Ptr Word8 -> Ptr Word8 -> Ptr AddrInfo -> Ptr (Ptr AddrInfo) -> IO CInt

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
#ifdef AF_UNIX
-- | local to host (pipes, portals
aF_UNIX             = SocketFamily (#const AF_UNIX            )
#endif
#ifdef AF_INET
-- | internetwork: UDP, TCP, etc
aF_INET             = SocketFamily (#const AF_INET            )
#endif
#ifdef AF_INET6
-- | Internet Protocol version 6
aF_INET6            = SocketFamily (#const AF_INET6           )
#endif
#ifdef AF_IMPLINK
-- | arpanet imp addresses
aF_IMPLINK          = SocketFamily (#const AF_IMPLINK         )
#endif
#ifdef AF_PUP
-- | pup protocols: e.g. BSP
aF_PUP              = SocketFamily (#const AF_PUP             )
#endif
#ifdef AF_CHAOS
-- | mit CHAOS protocols
aF_CHAOS            = SocketFamily (#const AF_CHAOS           )
#endif
#ifdef AF_NS
-- | XEROX NS protocols
aF_NS               = SocketFamily (#const AF_NS              )
#endif
#ifdef AF_NBS
-- | nbs protocols
aF_NBS              = SocketFamily (#const AF_NBS             )
#endif
#ifdef AF_ECMA
-- | european computer manufacturers
aF_ECMA             = SocketFamily (#const AF_ECMA            )
#endif
#ifdef AF_DATAKIT
-- | datakit protocols
aF_DATAKIT          = SocketFamily (#const AF_DATAKIT         )
#endif
#ifdef AF_CCITT
-- | CCITT protocols, X.25 etc
aF_CCITT            = SocketFamily (#const AF_CCITT           )
#endif
#ifdef AF_SNA
-- | IBM SNA
aF_SNA              = SocketFamily (#const AF_SNA             )
#endif
#ifdef AF_DECnet
-- | DECnet
aF_DECnet           = SocketFamily (#const AF_DECnet          )
#endif
#ifdef AF_DLI
-- | Direct data link interface
aF_DLI              = SocketFamily (#const AF_DLI             )
#endif
#ifdef AF_LAT
-- | LAT
aF_LAT              = SocketFamily (#const AF_LAT             )
#endif
#ifdef AF_HYLINK
-- | NSC Hyperchannel
aF_HYLINK           = SocketFamily (#const AF_HYLINK          )
#endif
#ifdef AF_APPLETALK
-- | Apple Talk
aF_APPLETALK        = SocketFamily (#const AF_APPLETALK       )
#endif
#ifdef AF_ROUTE
-- | Internal Routing Protocol
aF_ROUTE            = SocketFamily (#const AF_ROUTE           )
#endif
#ifdef AF_NETBIOS
-- | NetBios-style addresses
aF_NETBIOS          = SocketFamily (#const AF_NETBIOS         )
#endif
#ifdef AF_NIT
-- | Network Interface Tap
aF_NIT              = SocketFamily (#const AF_NIT             )
#endif
#ifdef AF_802
-- | IEEE 802.2, also ISO 8802
aF_802              = SocketFamily (#const AF_802             )
#endif
#ifdef AF_ISO
-- | ISO protocols
aF_ISO              = SocketFamily (#const AF_ISO             )
#endif
#ifdef AF_OSI
-- | umbrella of all families used by OSI
aF_OSI              = SocketFamily (#const AF_OSI             )
#endif
#ifdef AF_NETMAN
-- | DNA Network Management
aF_NETMAN           = SocketFamily (#const AF_NETMAN          )
#endif
#ifdef AF_X25
-- | CCITT X.25
aF_X25              = SocketFamily (#const AF_X25             )
#endif
#ifdef AF_AX25
aF_AX25             = SocketFamily (#const AF_AX25            )
#endif
#ifdef AF_OSINET
-- | AFI
aF_OSINET           = SocketFamily (#const AF_OSINET          )
#endif
#ifdef AF_GOSSIP
-- | US Government OSI
aF_GOSSIP           = SocketFamily (#const AF_GOSSIP          )
#endif
#ifdef AF_IPX
-- | Novell Internet Protocol
aF_IPX              = SocketFamily (#const AF_IPX             )
#endif
#ifdef Pseudo_AF_XTP
-- | eXpress Transfer Protocol (no AF)
pattern Pseudo_AF_XTP       = SocketFamily (#const Pseudo_AF_XTP      )
#endif
#ifdef AF_CTF
-- | Common Trace Facility
aF_CTF              = SocketFamily (#const AF_CTF             )
#endif
#ifdef AF_WAN
-- | Wide Area Network protocols
aF_WAN              = SocketFamily (#const AF_WAN             )
#endif
#ifdef AF_SDL
-- | SGI Data Link for DLPI
aF_SDL              = SocketFamily (#const AF_SDL             )
#endif
#ifdef AF_NETWARE
aF_NETWARE          = SocketFamily (#const AF_NETWARE         )
#endif
#ifdef AF_NDD
aF_NDD              = SocketFamily (#const AF_NDD             )
#endif
#ifdef AF_INTF
-- | Debugging use only
aF_INTF             = SocketFamily (#const AF_INTF            )
#endif
#ifdef AF_COIP
-- | connection-oriented IP, aka ST II
aF_COIP             = SocketFamily (#const AF_COIP            )
#endif
#ifdef AF_CNT
-- | Computer Network Technology
aF_CNT              = SocketFamily (#const AF_CNT             )
#endif
#ifdef Pseudo_AF_RTIP
-- | Help Identify RTIP packets
pattern Pseudo_AF_RTIP      = SocketFamily (#const Pseudo_AF_RTIP     )
#endif
#ifdef Pseudo_AF_PIP
-- | Help Identify PIP packets
pattern Pseudo_AF_PIP       = SocketFamily (#const Pseudo_AF_PIP      )
#endif
#ifdef AF_SIP
-- | Simple Internet Protocol
aF_SIP              = SocketFamily (#const AF_SIP             )
#endif
#ifdef AF_ISDN
-- | Integrated Services Digital Network
aF_ISDN             = SocketFamily (#const AF_ISDN            )
#endif
#ifdef Pseudo_AF_KEY
-- | Internal key-management function
pattern Pseudo_AF_KEY       = SocketFamily (#const Pseudo_AF_KEY      )
#endif
#ifdef AF_NATM
-- | native ATM access
aF_NATM             = SocketFamily (#const AF_NATM            )
#endif
#ifdef AF_ARP
-- | (rev.) addr. res. prot. (RFC 826)
aF_ARP              = SocketFamily (#const AF_ARP             )
#endif
#ifdef Pseudo_AF_HDRCMPLT
-- | Used by BPF to not rewrite hdrs in iface output
pattern Pseudo_AF_HDRCMPLT  = SocketFamily (#const Pseudo_AF_HDRCMPLT )
#endif
#ifdef AF_ENCAP
aF_ENCAP            = SocketFamily (#const AF_ENCAP           )
#endif
#ifdef AF_LINK
-- | Link layer interface
aF_LINK             = SocketFamily (#const AF_LINK            )
#endif
#ifdef AF_RAW
-- | Link layer interface
aF_RAW              = SocketFamily (#const AF_RAW             )
#endif
#ifdef AF_RIF
-- | raw interface
aF_RIF              = SocketFamily (#const AF_RIF             )
#endif
#ifdef AF_NETROM
-- | Amateur radio NetROM
aF_NETROM           = SocketFamily (#const AF_NETROM          )
#endif
#ifdef AF_BRIDGE
-- | multiprotocol bridge
aF_BRIDGE           = SocketFamily (#const AF_BRIDGE          )
#endif
#ifdef AF_ATMPVC
-- | ATM PVCs
aF_ATMPVC           = SocketFamily (#const AF_ATMPVC          )
#endif
#ifdef AF_ROSE
-- | Amateur Radio X.25 PLP
aF_ROSE             = SocketFamily (#const AF_ROSE            )
#endif
#ifdef AF_NETBEUI
-- | 802.2LLC
aF_NETBEUI          = SocketFamily (#const AF_NETBEUI         )
#endif
#ifdef AF_SECURITY
-- | Security callback pseudo AF
aF_SECURITY         = SocketFamily (#const AF_SECURITY        )
#endif
#ifdef AF_PACKET
-- | Packet family
aF_PACKET           = SocketFamily (#const AF_PACKET          )
#endif
#ifdef AF_ASH
-- | Ash
aF_ASH              = SocketFamily (#const AF_ASH             )
#endif
#ifdef AF_ECONET
-- | Acorn Econet
aF_ECONET           = SocketFamily (#const AF_ECONET          )
#endif
#ifdef AF_ATMSVC
-- | ATM SVCs
aF_ATMSVC           = SocketFamily (#const AF_ATMSVC          )
#endif
#ifdef AF_IRDA
-- | IRDA sockets
aF_IRDA             = SocketFamily (#const AF_IRDA            )
#endif
#ifdef AF_PPPOX
-- | PPPoX sockets
aF_PPPOX            = SocketFamily (#const AF_PPPOX           )
#endif
#ifdef AF_WANPIPE
-- | Wanpipe API sockets
aF_WANPIPE          = SocketFamily (#const AF_WANPIPE         )
#endif
#ifdef AF_BLUETOOTH
-- | bluetooth sockets
aF_BLUETOOTH        = SocketFamily (#const AF_BLUETOOTH       )
#endif
#ifdef AF_CAN
-- | Controller Area Network
aF_CAN              = SocketFamily (#const AF_CAN             )
#endif

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

protocol_TCP :: SocketProtocol
protocol_TCP = SocketProtocol (#const IPPROTO_TCP)

protocol_UDP :: SocketProtocol
protocol_UDP = SocketProtocol (#const IPPROTO_UDP)

