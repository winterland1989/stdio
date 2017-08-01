module System.IO.Socket.Errno where

import System.IO.Exception

#include "HsNet.h"
#if !defined(WITH_WINSOCK)
#include <errno.h>
#endif

throwSocketErrorIfNonZero :: 

-- | Contains the error code that can be matched against.
--
newtype SockErrno = SockErrno CInt
  deriving (Typeable, Eq, Ord)

instance IOErrno SockErrno
    showErrno = showSockErrno
    fromErrnoValue v = SockErrno v
    toErrnoValue (SockErrno v) = v

showSockErrno e
    | e == eOk                           = "eOk"
    | e == eInterrupted                  = "eInterrupted"
    | e == eBadFileDescriptor            = "eBadFileDescriptor"
    | e == eInvalid                      = "eInvalid"
    | e == ePipe                         = "ePipe"
    | e == eWouldBlock                   = "eWouldBlock"
    | e == eAgain                        = "eAgain"
    | e == eNotSocket                    = "eNotSocket"
    | e == eDestinationAddressRequired   = "eDestinationAddressRequired"
    | e == eMessageSize                  = "eMessageSize"
    | e == eProtocolType                 = "eProtocolType"
    | e == eNoProtocolOption             = "eNoProtocolOption"
    | e == eProtocolNotSupported         = "eProtocolNotSupported"
    | e == eSocketTypeNotSupported       = "eSocketTypeNotSupported"
    | e == eOperationNotSupported        = "eOperationNotSupported"
    | e == eProtocolFamilyNotSupported   = "eProtocolFamilyNotSupported"
    | e == eAddressFamilyNotSupported    = "eAddressFamilyNotSupported"
    | e == eAddressInUse                 = "eAddressInUse"
    | e == eAddressNotAvailable          = "eAddressNotAvailable"
    | e == eNetworkDown                  = "eNetworkDown"
    | e == eNetworkUnreachable           = "eNetworkUnreachable"
    | e == eNetworkReset                 = "eNetworkReset"
    | e == eConnectionAborted            = "eConnectionAborted"
    | e == eConnectionReset              = "eConnectionReset"
    | e == eNoBufferSpace                = "eNoBufferSpace"
    | e == eIsConnected                  = "eIsConnected"
    | e == eNotConnected                 = "eNotConnected"
    | e == eShutdown                     = "eShutdown"
    | e == eTooManyReferences            = "eTooManyReferences"
    | e == eTimedOut                     = "eTimedOut"
    | e == eConnectionRefused            = "eConnectionRefused"
    | e == eHostDown                     = "eHostDown"
    | e == eHostUnreachable              = "eHostUnreachable"
    | e == eAlready                      = "eAlready"
    | e == eInProgress                   = "eInProgress"
    | otherwise                          = let SockErrno n = e
                                           in "SockErrno " ++ show n

#if defined(WITH_WINSOCK)
#define SEOK                   0
#define SEINTR                 WSAEINTR
#define SEAGAIN                WSATRY_AGAIN
#define SEWOULDBLOCK           WSAEWOULDBLOCK
#define SEBADF                 WSAEBADF
#define SEINVAL                WSAEINVAL
#define SEINPROGRESS           WSAEINPROGRESS
#define SEPROTONOSUPPORT       WSAEPROTONOSUPPORT
#define SECONNREFUSED          WSAECONNREFUSED
#define SENETUNREACH           WSAENETUNREACH
#define SENOTCONN              WSAENOTCONN
#define SEALREADY              WSAEALREADY
#define SEISCONN               WSAEISCONN
#define SETIMEDOUT             WSAETIMEDOUT
#define SEPIPE                 WSAECONNABORTED
#define SEOPNOTSUPP            WSAEOPNOTSUPP
#define SENOTSOCK              WSAENOTSOCK
#define SEHOSTUNREACH          WSAEHOSTUNREACH
#define SEHOSTDOWN             WSAEHOSTDOWN
#define SETOOMANYREFS          WSAETOOMANYREFS
#define SESHUTDOWN             WSAESHUTDOWN
#define SENOBUFS               WSAENOBUFS
#define SENETRESET             WSAENETRESET
#define SENETDOWN              WSAENETDOWN
#define SECONNABORTED          WSAECONNABORTED
#define SECONNRESET            WSAECONNRESET
#define SEADDRNOTAVAIL         WSAEADDRNOTAVAIL
#define SEADDRINUSE            WSAEADDRINUSE
#define SEAFNOSUPPORT          WSAEAFNOSUPPORT
#define SEPFNOSUPPORT          WSAEPFNOSUPPORT
#define SESOCKTNOSUPPORT       WSAESOCKTNOSUPPORT
#define SENOPROTOOPT           WSAENOPROTOOPT
#define SEPROTOTYPE            WSAEPROTOTYPE
#define SEMSGSIZE              WSAEMSGSIZE
#define SEDESTADDRREQ          WSAEDESTADDRREQ
#else
#define SEOK                   0
#define SEINTR                 EINTR
#define SEAGAIN                EAGAIN
#define SEWOULDBLOCK           EWOULDBLOCK
#define SEBADF                 EBADF
#define SEINVAL                EINVAL
#define SEINPROGRESS           EINPROGRESS
#define SEPROTONOSUPPORT       EPROTONOSUPPORT
#define SECONNREFUSED          ECONNREFUSED
#define SENETUNREACH           ENETUNREACH
#define SENOTCONN              ENOTCONN
#define SEALREADY              EALREADY
#define SEISCONN               EISCONN
#define SETIMEDOUT             ETIMEDOUT
#define SEPIPE                 EPIPE
#define SEOPNOTSUPP            EOPNOTSUPP
#define SENOTSOCK              ENOTSOCK
#define SEDESTADDRREQ          EDESTADDRREQ
#define SEMSGSIZE              EMSGSIZE
#define SEPROTOTYPE            EPROTOTYPE
#define SENOPROTOOPT           ENOPROTOOPT
#define SESOCKTNOSUPPORT       ESOCKTNOSUPPORT
#define SEPFNOSUPPORT          EPFNOSUPPORT
#define SEAFNOSUPPORT          EAFNOSUPPORT
#define SEADDRINUSE            EADDRINUSE
#define SEADDRNOTAVAIL         EADDRNOTAVAIL
#define SENETDOWN              ENETDOWN
#define SENETRESET             ENETRESET
#define SECONNABORTED          ECONNABORTED
#define SECONNRESET            ECONNRESET
#define SENOBUFS               ENOBUFS
#define SESHUTDOWN             ESHUTDOWN
#define SETOOMANYREFS          ETOOMANYREFS
#define SEHOSTDOWN             EHOSTDOWN
#define SEHOSTUNREACH          EHOSTUNREACH
#endif


-- | > SockErrno "No error"
eOk                         :: SockErrno
eOk                          = SockErrno (#const SEOK)

-- | > SockErrno "Interrupted system call"
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eInterrupted                :: SockErrno
eInterrupted                 = SockErrno (#const SEINTR)

-- | > SockErrno "Bad file descriptor"
eBadFileDescriptor          :: SockErrno
eBadFileDescriptor           = SockErrno (#const SEBADF)

-- | > SockErrno "Invalid argument"
eInvalid                    :: SockErrno
eInvalid                     = SockErrno (#const SEINVAL)

-- | > SockErrno "Broken pipe"
ePipe                       :: SockErrno
ePipe                        = SockErrno (#const SEPIPE)

-- | > SockErrno "Resource temporarily unavailable"
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eWouldBlock                 :: SockErrno
eWouldBlock                  = SockErrno (#const SEWOULDBLOCK)

-- | > SockErrno "Resource temporarily unavailable"
eAgain                      :: SockErrno
eAgain                       = SockErrno (#const SEAGAIN)

-- | > SockErrno "Socket operation on non-socket"
--
--  NOTE: This should be ruled out by the type system.
eNotSocket                  :: SockErrno
eNotSocket                   = SockErrno (#const SENOTSOCK)

-- | > SockErrno "Destination address required"
eDestinationAddressRequired :: SockErrno
eDestinationAddressRequired  = SockErrno (#const SEDESTADDRREQ)

-- | > SockErrno "Message too long"
eMessageSize                :: SockErrno
eMessageSize                 = SockErrno (#const SEMSGSIZE)

-- | > SockErrno "Protocol wrong type for socket"

--  NOTE: This should be ruled out by the type system.
eProtocolType               :: SockErrno
eProtocolType                = SockErrno (#const SEPROTOTYPE)

-- | > SockErrno "Protocol not available"
eNoProtocolOption           :: SockErrno
eNoProtocolOption            = SockErrno (#const SENOPROTOOPT)

-- | > SockErrno "Protocol not supported"
eProtocolNotSupported       :: SockErrno
eProtocolNotSupported        = SockErrno (#const SEPROTONOSUPPORT)

-- | > SockErrno "Socket type not supported"
eSocketTypeNotSupported     :: SockErrno
eSocketTypeNotSupported      = SockErrno (#const SESOCKTNOSUPPORT)

-- | > SockErrno "Operation not supported"
eOperationNotSupported      :: SockErrno
eOperationNotSupported       = SockErrno (#const SEOPNOTSUPP)

-- | > SockErrno "Protocol family not supported"
eProtocolFamilyNotSupported :: SockErrno
eProtocolFamilyNotSupported  = SockErrno (#const SEPFNOSUPPORT)

-- | > SockErrno "Address family not supported by protocol"
eAddressFamilyNotSupported  :: SockErrno
eAddressFamilyNotSupported   = SockErrno (#const SEAFNOSUPPORT)

-- | > SockErrno "Address already in use"
eAddressInUse               :: SockErrno
eAddressInUse                = SockErrno (#const SEADDRINUSE)

-- | > SockErrno "Cannot assign requested address"
eAddressNotAvailable        :: SockErrno
eAddressNotAvailable         = SockErrno (#const SEADDRNOTAVAIL)

-- | > SockErrno "Network is down"
eNetworkDown                :: SockErrno
eNetworkDown                 = SockErrno (#const SENETDOWN)

-- | > SockErrno "Network is unreachable"
eNetworkUnreachable         :: SockErrno
eNetworkUnreachable          = SockErrno (#const SENETUNREACH)

-- | > SockErrno "Network dropped connection on reset"
eNetworkReset               :: SockErrno
eNetworkReset                = SockErrno (#const SENETRESET)

-- | > SockErrno "Software caused connection abort"
eConnectionAborted          :: SockErrno
eConnectionAborted           = SockErrno (#const SECONNABORTED)

-- | > SockErrno "Connection reset by peer"
eConnectionReset            :: SockErrno
eConnectionReset             = SockErrno (#const SECONNRESET)

-- | > SockErrno "No buffer space available"
eNoBufferSpace              :: SockErrno
eNoBufferSpace               = SockErrno (#const SENOBUFS)

-- | > SockErrno "Transport endpoint is already connected"
eIsConnected                :: SockErrno
eIsConnected                 = SockErrno (#const SEISCONN)

-- | > SockErrno "Transport endpoint is not connected"
eNotConnected               :: SockErrno
eNotConnected                = SockErrno (#const SENOTCONN)

-- | > SockErrno "Cannot send after transport endpoint shutdown"
eShutdown                   :: SockErrno
eShutdown                    = SockErrno (#const SESHUTDOWN)

-- | > SockErrno "Too many references: cannot splice"
eTooManyReferences          :: SockErrno
eTooManyReferences           = SockErrno (#const SETOOMANYREFS)

-- | > SockErrno "Connection timed out"
eTimedOut                   :: SockErrno
eTimedOut                    = SockErrno (#const SETIMEDOUT)

-- | > SockErrno "Connection refused"
eConnectionRefused          :: SockErrno
eConnectionRefused           = SockErrno (#const SECONNREFUSED)

-- | > SockErrno "Host is down"
eHostDown                   :: SockErrno
eHostDown                    = SockErrno (#const SEHOSTDOWN)

-- | > SockErrno "No route to host"
eHostUnreachable            :: SockErrno
eHostUnreachable             = SockErrno (#const SEHOSTUNREACH)

-- | > SockErrno "Operation already in progress"
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eAlready                    :: SockErrno
eAlready                     = SockErrno (#const SEALREADY)

-- | > SockErrno "Operation now in progress"
eInProgress                 :: SockErrno
eInProgress                  = SockErrno (#const SEINPROGRESS)

