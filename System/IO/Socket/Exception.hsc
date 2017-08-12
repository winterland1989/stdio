{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.Socket.Exception where

import System.IO.Exception
import Foreign.C.Types
import Foreign.C.String
import Data.Typeable
import GHC.Stack.Compat
import Control.Monad
import Network (withSocketsDo)
import Data.Bits
import Foreign.Storable

#include "HsNet.h"

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

#if defined(WITH_WINSOCK)
-- | The standard errno handling already take care of socket error on unix systems, but on windows
-- The errno is different, so does the errno handling.
--
newtype WSAReturn = WSAReturn CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

instance IOReturn WSAReturn where
    newtype IOErrno WSAReturn = WSAErrno CInt
        deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)
    isError (WSAReturn r) = r == -1
    isInterrupt e = False
    isBlock e = wSAEWOULDBLOCK
    getErrno _ == wsa_getLastError
    nameErrno = retur . nameWSAErrno
    descErrno e = c_getWSError e >>= peekCString
    throwErrno = throwWSAError

foreign import #{CALLCONV} unsafe "WSAGetLastError" wsa_getLastError :: IO (IOErrno WSAReturn)
foreign import ccall unsafe "getWSErrorDescr" c_getWSError :: IOErrno WSAReturn -> IO CString

--------------------------------------------------------------------------------

throwWSAError :: WSAErrno -> IOEInfo -> IO a
throwWSAError e info
    | e == wSAEINTR           = throwIO (Interrupted             info) -- TODO :: rework the mapping
    | e == wSAEBADF           = throwIO (InvalidArgument         info)
    | e == wSAEACCES          = throwIO (PermissionDenied        info)
    | e == wSAEFAULT          = throwIO (UnsupportedOperation    info)
    | e == wSAEINVAL          = throwIO (UnsupportedOperation    info)
    | e == wSAEMFILE          = throwIO (ResourceExhausted       info)
    | e == wSAEWOULDBLOCK     = throwIO (UnsupportedOperation    info)
    | e == wSAEINPROGRESS     = throwIO (ResourceExhausted       info)
    | e == wSAEALREADY        = throwIO (UnsupportedOperation    info)
    | e == wSAENOTSOCK        = throwIO (UnsupportedOperation    info)
    | e == wSAEDESTADDRREQ    = throwIO (ResourceVanished        info)
    | e == wSAEMSGSIZE        = throwIO (OtherError              info)
    | e == wSAEPROTOTYPE      = throwIO (UnsupportedOperation    info)
    | e == wSAENOPROTOOPT     = throwIO (ResourceExhausted       info)
    | e == wSAEPROTONOSUPPORT = throwIO (NoSuchThing             info)
    | e == wSAESOCKTNOSUPPORT = throwIO (NoSuchThing             info)
    | e == wSAEOPNOTSUPP      = throwIO (InvalidArgument         info)
    | e == wSAEPFNOSUPPORT    = throwIO (ProtocolError           info)
    | e == wSAEAFNOSUPPORT    = throwIO (UnsupportedOperation    info)
    | e == wSAEADDRINUSE      = throwIO (UnsupportedOperation    info)
    | e == wSAEADDRNOTAVAIL   = throwIO (AlreadyExists           info)
    | e == wSAENETDOWN        = throwIO (InvalidArgument         info)
    | e == wSAENETUNREACH     = throwIO (ResourceBusy            info)
    | e == wSAENETRESET       = throwIO (ResourceVanished        info)
    | e == wSAECONNABORTED    = throwIO (OtherError              info)
    | e == wSAECONNRESET      = throwIO (ResourceVanished        info)
    | e == wSAENOBUFS         = throwIO (NoSuchThing             info)
    | e == wSAEISCONN         = throwIO (ResourceVanished        info)
    | e == wSAENOTCONN        = throwIO (InvalidArgument         info)
    | e == wSAESHUTDOWN       = throwIO (AlreadyExists           info)
    | e == wSAETOOMANYREFS    = throwIO (OtherError              info)
    | e == wSAETIMEDOUT       = throwIO (PermissionDenied        info)
    | e == wSAECONNREFUSED    = throwIO (NoSuchThing             info)
    | e == wSAELOOP           = throwIO (Interrupted             info)
    | e == wSAENAMETOOLONG    = throwIO (InvalidArgument         info)
    | e == wSAEHOSTDOWN       = throwIO (HardwareFault           info)
    | e == wSAEHOSTUNREACH    = throwIO (AlreadyExists           info)
    | e == wSAENOTEMPTY       = throwIO (NoSuchThing             info)
    | e == wSAEPROCLIM        = throwIO (ResourceVanished        info)
    | e == wSAEUSERS          = throwIO (InvalidArgument         info)
    | e == wSAEDQUOT          = throwIO (AlreadyExists           info)
    | e == wSAESTALE          = throwIO (OtherError              info)
    | e == wSAEREMOTE         = throwIO (PermissionDenied        info)
    | e == wSAEDISCON         = throwIO (NoSuchThing             info)
    | e == wSASYSNOTREADY     = throwIO (Interrupted             info)
    | e == wSAVERNOTSUPPORTED = throwIO (InvalidArgument         info)
    | e == wSANOTINITIALISED  = throwIO (HardwareFault           info)
    | otherwise               = throwIO (OtherError              info)

nameWSAErrno :: IOErrno WSAReturn -> String
nameWSAErrno e
    | e == wSAEINTR            = "WSAEINTR"          
    | e == wSAEBADF            = "WSAEBADF"
    | e == wSAEACCES           = "WSAEACCES"
    | e == wSAEFAULT           = "WSAEFAULT"
    | e == wSAEINVAL           = "WSAEINVAL"
    | e == wSAEMFILE           = "WSAEMFILE"
    | e == wSAEWOULDBLOCK      = "WSAEWOULDBLOCK"
    | e == wSAEINPROGRESS      = "WSAEINPROGRESS"
    | e == wSAEALREADY         = "WSAEALREADY"
    | e == wSAENOTSOCK         = "WSAENOTSOCK"
    | e == wSAEDESTADDRREQ     = "WSAEDESTADDRREQ"
    | e == wSAEMSGSIZE         = "WSAEMSGSIZE"
    | e == wSAEPROTOTYPE       = "WSAEPROTOTYPE"
    | e == wSAENOPROTOOPT      = "WSAENOPROTOOPT"
    | e == wSAEPROTONOSUPPORT  = "WSAEPROTONOSUPPORT"
    | e == wSAESOCKTNOSUPPORT  = "WSAESOCKTNOSUPPORT"
    | e == wSAEOPNOTSUPP       = "WSAEOPNOTSUPP"
    | e == wSAEPFNOSUPPORT     = "WSAEPFNOSUPPORT"
    | e == wSAEAFNOSUPPORT     = "WSAEAFNOSUPPORT"
    | e == wSAEADDRINUSE       = "WSAEADDRINUSE"
    | e == wSAEADDRNOTAVAIL    = "WSAEADDRNOTAVAIL"
    | e == wSAENETDOWN         = "WSAENETDOWN"
    | e == wSAENETUNREACH      = "WSAENETUNREACH"
    | e == wSAENETRESET        = "WSAENETRESET"
    | e == wSAECONNABORTED     = "WSAECONNABORTED"
    | e == wSAECONNRESET       = "WSAECONNRESET"
    | e == wSAENOBUFS          = "WSAENOBUFS"
    | e == wSAEISCONN          = "WSAEISCONN"
    | e == wSAENOTCONN         = "WSAENOTCONN"
    | e == wSAESHUTDOWN        = "WSAESHUTDOWN"
    | e == wSAETOOMANYREFS     = "WSAETOOMANYREFS"
    | e == wSAETIMEDOUT        = "WSAETIMEDOUT"
    | e == wSAECONNREFUSED     = "WSAECONNREFUSED"
    | e == wSAELOOP            = "WSAELOOP"
    | e == wSAENAMETOOLONG     = "WSAENAMETOOLONG"
    | e == wSAEHOSTDOWN        = "WSAEHOSTDOWN"
    | e == wSAEHOSTUNREACH     = "WSAEHOSTUNREACH"
    | e == wSAENOTEMPTY        = "WSAENOTEMPTY"
    | e == wSAEPROCLIM         = "WSAEPROCLIM"
    | e == wSAEUSERS           = "WSAEUSERS"
    | e == wSAEDQUOT           = "WSAEDQUOT"
    | e == wSAESTALE           = "WSAESTALE"
    | e == wSAEREMOTE          = "WSAEREMOTE"
    | e == wSAEDISCON          = "WSAEDISCON"
    | e == wSASYSNOTREADY      = "WSASYSNOTREADY"
    | e == wSAVERNOTSUPPORTED  = "WSAVERNOTSUPPORTED"
    | e == wSANOTINITIALISED   = "WSANOTINITIALISED"
    | otherwise                = show e


wSAEINTR           = WSAErrno (#const WSAEINTR  )
wSAEBADF           = WSAErrno (#const WSAEBADF  )
wSAEACCES          = WSAErrno (#const WSAEACCES )
wSAEFAULT          = WSAErrno (#const WSAEFAULT )
wSAEINVAL          = WSAErrno (#const WSAEINVAL )
wSAEMFILE          = WSAErrno (#const WSAEMFILE )
wSAEWOULDBLOCK     = WSAErrno (#const WSAEWOULDBLOCK  )
wSAEINPROGRESS     = WSAErrno (#const WSAEINPROGRESS  )
wSAEALREADY        = WSAErrno (#const WSAEALREADY     )
wSAENOTSOCK        = WSAErrno (#const WSAENOTSOCK     )
wSAEDESTADDRREQ    = WSAErrno (#const WSAEDESTADDRREQ )
wSAEMSGSIZE        = WSAErrno (#const WSAEMSGSIZE    	)
wSAEPROTOTYPE      = WSAErrno (#const WSAEPROTOTYPE   )
wSAENOPROTOOPT     = WSAErrno (#const WSAENOPROTOOPT  )
wSAEPROTONOSUPPORT = WSAErrno (#const WSAEPROTONOSUPPORT )
wSAESOCKTNOSUPPORT = WSAErrno (#const WSAESOCKTNOSUPPORT )
wSAEOPNOTSUPP      = WSAErrno (#const WSAEOPNOTSUPP      )
wSAEPFNOSUPPORT    = WSAErrno (#const WSAEPFNOSUPPORT    )
wSAEAFNOSUPPORT    = WSAErrno (#const WSAEAFNOSUPPORT    )
wSAEADDRINUSE      = WSAErrno (#const WSAEADDRINUSE      )
wSAEADDRNOTAVAIL   = WSAErrno (#const WSAEADDRNOTAVAIL   )
wSAENETDOWN        = WSAErrno (#const WSAENETDOWN        )
wSAENETUNREACH     = WSAErrno (#const WSAENETUNREACH     )
wSAENETRESET       = WSAErrno (#const WSAENETRESET       )
wSAECONNABORTED    = WSAErrno (#const WSAECONNABORTED    )
wSAECONNRESET      = WSAErrno (#const WSAECONNRESET      )
wSAENOBUFS         = WSAErrno (#const WSAENOBUFS         )
wSAEISCONN         = WSAErrno (#const WSAEISCONN         )
wSAENOTCONN        = WSAErrno (#const WSAENOTCONN        )
wSAESHUTDOWN       = WSAErrno (#const WSAESHUTDOWN       )
wSAETOOMANYREFS    = WSAErrno (#const WSAETOOMANYREFS    )
wSAETIMEDOUT       = WSAErrno (#const WSAETIMEDOUT       )
wSAECONNREFUSED    = WSAErrno (#const WSAECONNREFUSED    )
wSAELOOP           = WSAErrno (#const WSAELOOP           )
wSAENAMETOOLONG    = WSAErrno (#const WSAENAMETOOLONG    )
wSAEHOSTDOWN       = WSAErrno (#const WSAEHOSTDOWN       )
wSAEHOSTUNREACH    = WSAErrno (#const WSAEHOSTUNREACH    )
wSAENOTEMPTY       = WSAErrno (#const WSAENOTEMPTY       )
wSAEPROCLIM        = WSAErrno (#const WSAEPROCLIM        )
wSAEUSERS          = WSAErrno (#const WSAEUSERS          )
wSAEDQUOT          = WSAErrno (#const WSAEDQUOT          )
wSAESTALE          = WSAErrno (#const WSAESTALE          )
wSAEREMOTE         = WSAErrno (#const WSAEREMOTE         )
wSAEDISCON         = WSAErrno (#const WSAEDISCON         )
wSASYSNOTREADY     = WSAErrno (#const WSASYSNOTREADY     )
wSAVERNOTSUPPORTED = WSAErrno (#const WSAVERNOTSUPPORTED )
wSANOTINITIALISED  = WSAErrno (#const WSANOTINITIALISED  )

#endif

--------------------------------------------------------------------------------

newtype AddrInfoReturn a = AddrInfoReturn a
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

instance IOReturn AddrInfoReturn where
    newtype IOErrno AddrInfoReturn = AddrInfoErrno CInt
        deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)
    isError (AddrInfoReturn r) = r /= 0
    isInterrupt _ = False
    isBlock _ = False
    getReturn (AddrInfoReturn r) = r
    getErrno (AddrInfoReturn r) =  return (AddrInfoErrno (fromIntegral r))
    nameErrno = return . nameAddrInfoErrno
    descErrno e = gai_strerror e
    throwErrno = throwAddrInfoError

gai_strerror :: IOErrno AddrInfoReturn -> IO String
#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror" c_gai_strerror :: IOErrno AddrInfoReturn -> IO CString
#else
gai_strerror _ = return "gai_strerror not supported on your platform"
#endif

-- | Wrapper `getAddrInfo/getNameInfo` functions and throw appropriate exceptions.
-- 
throwAddrInfoError :: IOErrno AddrInfoReturn -> IOEInfo -> IO a
throwAddrInfoError e info
    | e == eAI_ADDRFAMILY   = throwIO (UnsupportedOperation info)
    | e == eAI_AGAIN        = throwIO (ResourceExhausted info)
    | e == eAI_BADFLAGS     = throwIO (UnsupportedOperation info)
    | e == eAI_FAIL         = throwIO (OtherError info)
    | e == eAI_FAMILY       = throwIO (UnsupportedOperation info)
    | e == eAI_MEMORY       = throwIO (ResourceExhausted info)
    | e == eAI_NONAME       = throwIO (NoSuchThing info)
    | e == eAI_SERVICE      = throwIO (UnsupportedOperation info)
    | e == eAI_SOCKTYPE     = throwIO (UnsupportedOperation info)
    | e == eAI_STSTEM       = throwIO (SystemError info)
    | otherwise             = throwIO (OtherError info)

nameAddrInfoErrno :: IOErrno AddrInfoReturn -> String
nameAddrInfoErrno e
    | e == eAI_ADDRFAMILY   = "EAI_ADDRFAMILY"
    | e == eAI_AGAIN        = "EAI_AGAIN"
    | e == eAI_BADFLAGS     = "EAI_BADFLAGS"
    | e == eAI_FAIL         = "EAI_FAIL"
    | e == eAI_FAMILY       = "EAI_FAMILY"
    | e == eAI_MEMORY       = "EAI_MEMORY"
    | e == eAI_NONAME       = "EAI_NONAME"
    | e == eAI_SERVICE      = "EAI_SERVICE"
    | e == eAI_SOCKTYPE     = "EAI_SOCKTYPE"
    | e == eAI_STSTEM       = "EAI_STSTEM"
    | otherwise             = show e


eAI_ADDRFAMILY :: IOErrno AddrInfoReturn
#if defined(eAI_ADDRFAMILY)
eAI_ADDRFAMILY = AddrInfoErrno (#const EAI_ADDRFAMILY)
#else
eAI_ADDRFAMILY = AddrInfoErrno 0    -- windows doesn't have this
#endif

-- | > AddrInfoErrno "Temporary failure in name resolution"
eAI_AGAIN    :: IOErrno AddrInfoReturn
eAI_AGAIN     = AddrInfoErrno (#const EAI_AGAIN)

-- | > AddrInfoErrno "Bad value for ai_flags"
eAI_BADFLAGS :: IOErrno AddrInfoReturn
eAI_BADFLAGS  = AddrInfoErrno (#const EAI_BADFLAGS)

-- | > AddrInfoErrno "Non-recoverable failure in name resolution"
eAI_FAIL     :: IOErrno AddrInfoReturn
eAI_FAIL      = AddrInfoErrno (#const EAI_FAIL)

-- | > AddrInfoErrno "ai_family not supported"
eAI_FAMILY   :: IOErrno AddrInfoReturn
eAI_FAMILY    = AddrInfoErrno (#const EAI_FAMILY)

-- | > AddrInfoErrno "Memory allocation failure"
eAI_MEMORY   :: IOErrno AddrInfoReturn
eAI_MEMORY    = AddrInfoErrno (#const EAI_MEMORY)

-- | > AddrInfoErrno "No such host is known"
eAI_NONAME   :: IOErrno AddrInfoReturn
eAI_NONAME    = AddrInfoErrno (#const EAI_NONAME)

-- | > AddrInfoErrno "Servname not supported for ai_socktype"
eAI_SERVICE  :: IOErrno AddrInfoReturn
eAI_SERVICE   = AddrInfoErrno (#const EAI_SERVICE)

-- | > AddrInfoErrno "ai_socktype not supported"
eAI_SOCKTYPE :: IOErrno AddrInfoReturn
eAI_SOCKTYPE  = AddrInfoErrno (#const EAI_SOCKTYPE)

-- | > AddrInfoErrno "System error"
eAI_STSTEM   :: IOErrno AddrInfoReturn
#if defined(EAI_SYSTEM) 
eAI_STSTEM    = AddrInfoErrno (#const EAI_SYSTEM)
#else
eAI_STSTEM    = AddrInfoErrno 0     -- windows doesn't have this
#endif

