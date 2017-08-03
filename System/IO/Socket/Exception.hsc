{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Exception 
  ( throwAddrErrorIfNonZero
  , throwSocketErrorIfMinus1Retry
  ) where

import System.IO.Exception
import Foreign.C.Types
import Foreign.C.String
import Data.Typeable
import GHC.Stack.Compat
import Control.Monad

#include "HsNet.h"


#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

throwSocketErrorIfMinus1Retry :: CallStack -> String -> IO CInt -> IO CInt
throwSocketErrorIfMinus1Retry cstack dev f = do
#if defined(WITH_WINSOCK)
    r <- f
    if (r == -1)
    then do
        rc <- wsa_getLastError
        case rc of
            #{const WSANOTINITIALISED} -> do
                withSocketsDo (return ())
                r <- act
                if (r == -1)
                then do
                    rc <- wsa_getLastError
                    throwWinSocketError (WSAErrno rc) cstack dev
                else return r
            _ -> throwWinSocketError (WSAErrno rc) cstack dev
    else return r
#else
    throwErrnoIfMinus1Retry cstack dev f
#endif
    

#if defined(WITH_WINSOCK)
-- | The standard errno handling already take care of socket error on unix systems, but on windows
-- The errno is different, so does the errno handling.
--
newtype WSAErrno = WSAErrno CInt deriving (Typeable, Eq, Ord)

instance IOErrno WSAErrno
    showErrno = showWSAErrno
    fromErrnoValue v = WSAErrno v
    toErrnoValue (WSAErrno v) = v

foreign import #{WINDOWS_CCONV} unsafe "WSAGetLastError" wsa_getLastError :: IO CInt
foreign import ccall unsafe "getWSErrorDescr" c_getWSError :: CInt -> IO (Ptr CChar)

throwWinSocketError :: WSAErrno -> CallStack -> String -> IO ()
throwWinSocketError errno@(WSAErrno e) cstack dev = do
    desc <- c_getWSError e >>= peekCString
    let info IOEInfo = IOEInfo errno desc dev cstack
    case () of
        _                                                                              
            | errno == wSAEINTR           -> throwIO (Interrupted             info) -- TODO :: rework the mapping
            | errno == wSAEBADF           -> throwIO (InvalidArgument         info)
            | errno == wSAEACCES          -> throwIO (PermissionDenied        info)
            | errno == wSAEFAULT          -> throwIO (UnsupportedOperation    info)
            | errno == wSAEINVAL          -> throwIO (UnsupportedOperation    info)
            | errno == wSAEMFILE          -> throwIO (ResourceExhausted       info)
            | errno == wSAEWOULDBLOCK     -> throwIO (UnsupportedOperation    info)
            | errno == wSAEINPROGRESS     -> throwIO (ResourceExhausted       info)
            | errno == wSAEALREADY        -> throwIO (UnsupportedOperation    info)
            | errno == wSAENOTSOCK        -> throwIO (UnsupportedOperation    info)
            | errno == wSAEDESTADDRREQ    -> throwIO (ResourceVanished        info)
            | errno == wSAEMSGSIZE        -> throwIO (OtherError              info)
            | errno == wSAEPROTOTYPE      -> throwIO (UnsupportedOperation    info)
            | errno == wSAENOPROTOOPT     -> throwIO (ResourceExhausted       info)
            | errno == wSAEPROTONOSUPPORT -> throwIO (NoSuchThing             info)
            | errno == wSAESOCKTNOSUPPORT -> throwIO (NoSuchThing             info)
            | errno == wSAEOPNOTSUPP      -> throwIO (InvalidArgument         info)
            | errno == wSAEPFNOSUPPORT    -> throwIO (ProtocolError           info)
            | errno == wSAEAFNOSUPPORT    -> throwIO (UnsupportedOperation    info)
            | errno == wSAEADDRINUSE      -> throwIO (UnsupportedOperation    info)
            | errno == wSAEADDRNOTAVAIL   -> throwIO (AlreadyExists           info)
            | errno == wSAENETDOWN        -> throwIO (InvalidArgument         info)
            | errno == wSAENETUNREACH     -> throwIO (ResourceBusy            info)
            | errno == wSAENETRESET       -> throwIO (ResourceVanished        info)
            | errno == wSAECONNABORTED    -> throwIO (OtherError              info)
            | errno == wSAECONNRESET      -> throwIO (ResourceVanished        info)
            | errno == wSAENOBUFS         -> throwIO (NoSuchThing             info)
            | errno == wSAEISCONN         -> throwIO (ResourceVanished        info)
            | errno == wSAENOTCONN        -> throwIO (InvalidArgument         info)
            | errno == wSAESHUTDOWN       -> throwIO (AlreadyExists           info)
            | errno == wSAETOOMANYREFS    -> throwIO (OtherError              info)
            | errno == wSAETIMEDOUT       -> throwIO (PermissionDenied        info)
            | errno == wSAECONNREFUSED    -> throwIO (NoSuchThing             info)
            | errno == wSAELOOP           -> throwIO (Interrupted             info)
            | errno == wSAENAMETOOLONG    -> throwIO (InvalidArgument         info)
            | errno == wSAEHOSTDOWN       -> throwIO (HardwareFault           info)
            | errno == wSAEHOSTUNREACH    -> throwIO (AlreadyExists           info)
            | errno == wSAENOTEMPTY       -> throwIO (NoSuchThing             info)
            | errno == wSAEPROCLIM        -> throwIO (ResourceVanished        info)
            | errno == wSAEUSERS          -> throwIO (InvalidArgument         info)
            | errno == wSAEDQUOT          -> throwIO (AlreadyExists           info)
            | errno == wSAESTALE          -> throwIO (OtherError              info)
            | errno == wSAEREMOTE         -> throwIO (PermissionDenied        info)
            | errno == wSAEDISCON         -> throwIO (NoSuchThing             info)
            | errno == wSASYSNOTREADY     -> throwIO (Interrupted             info)
            | errno == wSAVERNOTSUPPORTED -> throwIO (InvalidArgument         info)
            | errno == wSANOTINITIALISED  -> throwIO (HardwareFault           info)



--------------------------------------------------------------------------------

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

newtype AddrInfoErrno = AddrInfoErrno CInt deriving (Typeable, Eq, Typeable)

instance IOErrno AddrInfoErrno where
    showErrno = showAddrIntoErrno
    fromErrnoValue errno = AddrInfoErrno errno 
    toErrnoValue (AddrInfoErrno errno) = errno

-- | Wrapper `getAddrInfo/getNameInfo` functions and throw appropriate exceptions.
-- 
throwAddrErrorIfNonZero :: CallStack -> String -> IO CInt -> IO ()
throwAddrErrorIfNonZero cstack dev f = do
    r <- f
    when (r /= 0) $ do
        desc <- gai_strerror r
        let errno = AddrInfoErrno r
            info = IOEInfo errno desc dev cstack
        case () of 
            _
                | errno == eAI_AGAIN        -> throwIO (ResourceExhausted info)
                | errno == eAI_BADFLAGS     -> throwIO (UnsupportedOperation info)
                | errno == eAI_FAIL         -> throwIO (OtherError info)
                | errno == eAI_FAMILY       -> throwIO (UnsupportedOperation info)
                | errno == eAI_MEMORY       -> throwIO (ResourceExhausted info)
                | errno == eAI_NONAME       -> throwIO (NoSuchThing info)
                | errno == eAI_SERVICE      -> throwIO (UnsupportedOperation info)
                | errno == eAI_SOCKTYPE     -> throwIO (UnsupportedOperation info)
                | errno == eAI_STSTEM       -> throwIO (SystemError info)

showAddrIntoErrno :: AddrInfoErrno -> String
showAddrIntoErrno errno
    | errno == eAI_AGAIN        = "EAI_AGAIN"
    | errno == eAI_BADFLAGS     = "EAI_BADFLAGS"
    | errno == eAI_FAIL         = "EAI_FAIL"
    | errno == eAI_FAMILY       = "EAI_FAMILY"
    | errno == eAI_MEMORY       = "EAI_MEMORY"
    | errno == eAI_NONAME       = "EAI_NONAME"
    | errno == eAI_SERVICE      = "EAI_SERVICE"
    | errno == eAI_SOCKTYPE     = "EAI_SOCKTYPE"
    | errno == eAI_STSTEM       = "EAI_SYSTEM"

-- | > AddrInfoErrno "Temporary failure in name resolution"
eAI_AGAIN    :: AddrInfoErrno
eAI_AGAIN     = AddrInfoErrno (#const EAI_AGAIN)

-- | > AddrInfoErrno "Bad value for ai_flags"
eAI_BADFLAGS :: AddrInfoErrno
eAI_BADFLAGS  = AddrInfoErrno (#const EAI_BADFLAGS)

-- | > AddrInfoErrno "Non-recoverable failure in name resolution"
eAI_FAIL     :: AddrInfoErrno
eAI_FAIL      = AddrInfoErrno (#const EAI_FAIL)

-- | > AddrInfoErrno "ai_family not supported"
eAI_FAMILY   :: AddrInfoErrno
eAI_FAMILY    = AddrInfoErrno (#const EAI_FAMILY)

-- | > AddrInfoErrno "Memory allocation failure"
eAI_MEMORY   :: AddrInfoErrno
eAI_MEMORY    = AddrInfoErrno (#const EAI_MEMORY)

-- | > AddrInfoErrno "No such host is known"
eAI_NONAME   :: AddrInfoErrno
eAI_NONAME    = AddrInfoErrno (#const EAI_NONAME)

-- | > AddrInfoErrno "Servname not supported for ai_socktype"
eAI_SERVICE  :: AddrInfoErrno
eAI_SERVICE   = AddrInfoErrno (#const EAI_SERVICE)

-- | > AddrInfoErrno "ai_socktype not supported"
eAI_SOCKTYPE :: AddrInfoErrno
eAI_SOCKTYPE  = AddrInfoErrno (#const EAI_SOCKTYPE)

-- | > AddrInfoErrno "System error"
eAI_STSTEM   :: AddrInfoErrno
eAI_STSTEM    = AddrInfoErrno (#const EAI_SYSTEM)

gai_strerror :: CInt -> IO String
#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror" c_gai_strerror :: CInt -> IO CString
#else
gai_strerror _ = return "gai_strerror not supported on your platform"
#endif
