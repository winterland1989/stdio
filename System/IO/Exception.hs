{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : System.IO.Exception
Description : Extensible IO exceptions
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module implemented extensible io exception following approach described in /An Extensible Dynamically-Typed
Hierarchy of Exceptions/ by Simon Marlow. The implementation in this module has simplified to meet common need.
User who want to catch certain type of exceptions can directly use exception types this module provide,
which are modeled after @IOErrorType@ from "GHC.IO.Exception". This module also provide similar functions from
"Foreign.C.Error" with a different naming scheme, the old naming is too long and incomprehensible.

Functions from this package will throw exceptions from this module only instead of the old 'IOError' on I/O exceptions.

Example for library author defining new io exception:

@
  data MyNetworkException = MyNetworkException ... deriving (Show, Typeable)
  instance Exception MyNetworkException where
        toException = ioExceptionToException
        fromException = ioExceptionFromException
@

Exceptions from this module contain 'IOEInfo' which is pretty detailed, but this also require user of this module
do some extra work to keep error message's quality(provide CallStack, device informations, etc.).
New defined I/O exceptions are encouraged to include a 'IOEInfo', since it helps a lot when debugging.

-}

module System.IO.Exception
  ( -- * The 'SomeIOException' type
    SomeIOException(..)
  , ioExceptionToException
  , ioExceptionFromException
    -- * Builtin io exception types
  , IOEInfo(..)
  , AlreadyExists(..)
  , NoSuchThing(..)
  , ResourceBusy(..)
  , ResourceExhausted(..)
  , EOF(..)
  , IllegalOperation(..)
  , PermissionDenied(..)
  , UnsatisfiedConstraints(..)
  , SystemError(..)
  , ProtocolError(..)
  , OtherError(..)
  , InvalidArgument(..)
  , InappropriateType(..)
  , HardwareFault(..)
  , UnsupportedOperation(..)
  , TimeExpired(..)
  , ResourceVanished(..)
  , Interrupted(..)
    -- * Throw io exceptions
  , throwIO
  , throwOOMIfNull
    -- * Default errno type
  , IOReturn(..)
  , throwIfError
  , retryInterrupt
  , retryInterruptWaitBlock
   -- * unix return and errno
  , UnixReturn(..)
  , eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN,
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED,
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT,
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ,
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK,
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH,
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK,
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS,
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTSUP, eNOTTY, eNXIO,
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL,
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE,
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN,
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT,
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
  ) where


-- this is were we get the CONST_XXX definitions from that configure
-- calculated for us
--
#include "HsBaseConfig.h"

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import GHC.Stack.Compat
import Data.Bits
import Foreign.Storable

-- | The root type of all io exceptions, you can catch all io exception by catching this root type.
--
data SomeIOException = forall e . Exception e => SomeIOException e
    deriving Typeable

instance Show SomeIOException where
    show (SomeIOException e) = show e

instance Exception SomeIOException

ioExceptionToException :: Exception e => e -> SomeException
ioExceptionToException = toException . SomeIOException

ioExceptionFromException :: Exception e => SomeException -> Maybe e
ioExceptionFromException x = do
    SomeIOException a <- fromException x
    cast a

#define IOE(e) data e = e IOEInfo deriving (Show, Typeable);  \
               instance Exception e where                     \
                   { toException = ioExceptionToException     \
                   ; fromException = ioExceptionFromException \
                   }
IOE(AlreadyExists)
IOE(NoSuchThing)
IOE(ResourceBusy)
IOE(ResourceExhausted)
IOE(EOF)
IOE(IllegalOperation)
IOE(PermissionDenied)
IOE(UnsatisfiedConstraints)
IOE(SystemError)
IOE(ProtocolError)
IOE(OtherError)
IOE(InvalidArgument)
IOE(InappropriateType)
IOE(HardwareFault)
IOE(UnsupportedOperation)
IOE(TimeExpired)
IOE(ResourceVanished)
IOE(Interrupted)

--------------------------------------------------------------------------------

-- | Throw 'ResourceExhausted' if action return a 'nullPtr'.
--
throwOOMIfNull :: HasCallStack
               => String        -- ^ a descrition of what you are doing
               -> IO (Ptr a)    -- ^ the action
               -> IO (Ptr a)
throwOOMIfNull info f = do
   addr <- f
   if addr == nullPtr
      then throwIO (ResourceExhausted (IOEInfo "" "out of memory" info callStack))
      else return addr

--------------------------------------------------------------------------------

-- | This class should be used to mark FFI return value with certain errno mechanism.
--
-- For example, 'UnixReturn CInt' should be used instead of 'CInt' if you are dealing
-- with unix system calls which will return @-1@ on error and mark errno from @errno.h@.
--
-- Some calls never return interrupt or blocking errnos, just return 'False' in that case.
--
class Integral (IOErrno r) => IOReturn r where
    -- | The errno type associated with 'r'
    data IOErrno r :: *
    -- | Is this return value indicate an error?
    isError   :: Integral a => r a -> Bool
    -- | How can i get a errno then?
    getErrno  :: Integral a => r a -> IO (IOErrno r)
    -- | Is this errno indicate interrupted blocking call?
    isInterrupt :: IOErrno r -> Bool
    -- | Is this errno indicate no data on a non-blocking device?
    isBlock   :: IOErrno r -> Bool
    -- | OK, i want my return value if no errno.
    getReturn :: Integral a => r a -> a
    -- | Read the errno name for me.
    nameErrno :: IOErrno r -> IO String
    -- | Read the errno description for me.
    descErrno :: IOErrno r -> IO String
    -- | I can prepare 'IOEInfo' for you with name and description above,
    -- but which exactly 'IOException' you want to throw?
    throwErrno :: IOErrno r -> IOEInfo -> IO a

-- | Throw if 'isError' say so.
--
throwIfError :: (HasCallStack, IOReturn r, Integral a) => String -> IO (r a) -> IO a
throwIfError dev f = do
    r <- f
    if (isError r)
    then do
        e <- getErrno r
        name <- nameErrno e
        desc <- descErrno e
        throwErrno e (IOEInfo name desc dev callStack)
    else return (getReturn r)

-- | Like 'throwIfError', but retry if 'isInterrupt' return true.
--
retryInterrupt :: (HasCallStack, IOReturn r, Integral a) => String -> IO (r a) -> IO a
retryInterrupt dev f = do
    r <- f
    if (isError r)
    then do
        e <- getErrno r
        if (isInterrupt e)
        then retryInterrupt dev f
        else do
            name <- nameErrno e
            desc <- descErrno e
            throwErrno e (IOEInfo name desc dev callStack)
    else return (getReturn r)

-- | Like 'retryInterrupt', but try a different action if 'isBlock' return true.
--
-- The second action should handle errno on its own, we directly ask for its result.
--
retryInterruptWaitBlock :: (HasCallStack, IOReturn r, Integral a)
                        => String -> IO (r a) -> IO a -> IO a
retryInterruptWaitBlock dev f f2 = f >>= loop
  where
    loop r =
        if (isError r)
        then do
            e <- getErrno r
            if isInterrupt e
            then do
                retryInterruptWaitBlock dev f f2
            else if isBlock e
                then f2
                else do
                    name <- nameErrno e
                    desc <- descErrno e
                    throwErrno e (IOEInfo name desc dev callStack)
        else return (getReturn r)

-- | IO exceptions informations.
--
data IOEInfo = IOEInfo
    { ioeErrno       :: String      -- ^ the errno name, e.g. EACCESS, UV_EADDRINUSE, WSAECONNRESET, etc. empty if no errno.
    , ioeDescription :: String      -- ^ description for this io error, can be errno description, or some custom description if no errno.
    , ioeDevice      :: String      -- ^ device info, such as filename, socket address, etc
    , ioeCallStack   :: CallStack   -- ^ lightweight partial call-stack
    }

instance Show IOEInfo where
    show (IOEInfo [] desc dev cstack) =
         "{description:" ++ desc ++
         ", device:" ++ dev ++
         ", callstack:" ++ prettyCallStack cstack ++ "}"
    show (IOEInfo errno desc dev cstack) =
         "{errno:" ++ errno ++
         ", description:" ++ desc ++
         ", device:" ++ dev ++
         ", callstack:" ++ prettyCallStack cstack ++ "}"

newtype UnixReturn a = UnixReturn a
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

instance IOReturn UnixReturn where
    newtype IOErrno UnixReturn = UnixErrno CInt
        deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)
    isError (UnixReturn r) = r == (-1)
    isInterrupt e = e == eINTR
    isBlock e = e == eAGAIN || e == eWOULDBLOCK
    getErrno _ = get_errno
    getReturn (UnixReturn r) = r
    nameErrno = return . nameUnixErrno
    descErrno e = strerror e >>= peekCString
    throwErrno = throwUnixError

foreign import ccall unsafe "string.h" strerror :: IOErrno UnixReturn -> IO CString
foreign import ccall unsafe "HsBase.h __hscore_get_errno" get_errno :: IO (IOErrno UnixReturn)

nameUnixErrno :: IOErrno UnixReturn -> String
nameUnixErrno e
    | e == eOK             = "EOK"
    | e == e2BIG           = "E2BIG"
    | e == eACCES          = "EACCES"
    | e == eADDRINUSE      = "EADDRINUSE"
    | e == eADDRNOTAVAIL   = "EADDRNOTAVAIL"
    | e == eADV            = "EADV"
    | e == eAFNOSUPPORT    = "EAFNOSUPPORT"
    | e == eAGAIN          = "EAGAIN"
    | e == eALREADY        = "EADY"
    | e == eBADF           = "EBADF"
    | e == eBADMSG         = "EBADMSG"
    | e == eBADRPC         = "EBADRPC"
    | e == eBUSY           = "EBUSY"
    | e == eCHILD          = "ECHILD"
    | e == eCOMM           = "ECOMM"
    | e == eCONNABORTED    = "ECONNABORTED"
    | e == eCONNREFUSED    = "ECONNREFUSED"
    | e == eCONNRESET      = "ECONNRESET"
    | e == eDEADLK         = "EDEADLK"
    | e == eDESTADDRREQ    = "EDESTADDRREQ"
    | e == eDIRTY          = "EDIRTY"
    | e == eDOM            = "EDOM"
    | e == eDQUOT          = "EDQUOT"
    | e == eEXIST          = "EEXIST"
    | e == eFAULT          = "EFAULT"
    | e == eFBIG           = "EFBIG"
    | e == eFTYPE          = "EFTYPE"
    | e == eHOSTDOWN       = "EHOSTDOWN"
    | e == eHOSTUNREACH    = "EHOSTUNREACH"
    | e == eIDRM           = "EIDRM"
    | e == eILSEQ          = "EILSEQ"
    | e == eINPROGRESS     = "EINPROGRESS"
    | e == eINTR           = "EINTR"
    | e == eINVAL          = "EINVAL"
    | e == eIO             = "EIO"
    | e == eISCONN         = "EISCONN"
    | e == eISDIR          = "EISDIR"
    | e == eLOOP           = "ELOOP"
    | e == eMFILE          = "EMFILE"
    | e == eMLINK          = "EMLINK"
    | e == eMSGSIZE        = "EMSGSIZE"
    | e == eMULTIHOP       = "EMULTIHOP"
    | e == eNAMETOOLONG    = "ENAMETOOLONG"
    | e == eNETDOWN        = "ENETDOWN"
    | e == eNETRESET       = "ENETRESET"
    | e == eNETUNREACH     = "ENETUNREACH"
    | e == eNFILE          = "ENFILE"
    | e == eNOBUFS         = "ENOBUFS"
    | e == eNODATA         = "ENODATA"
    | e == eNODEV          = "ENODEV"
    | e == eNOENT          = "ENOENT"
    | e == eNOEXEC         = "ENOEXEC"
    | e == eNOLCK          = "ENOLCK"
    | e == eNOLINK         = "ENOLINK"
    | e == eNOMEM          = "ENOMEM"
    | e == eNOMSG          = "ENOMSG"
    | e == eNONET          = "ENONET"
    | e == eNOPROTOOPT     = "ENOPROTOOPT"
    | e == eNOSPC          = "ENOSPC"
    | e == eNOSR           = "ENOSR"
    | e == eNOSTR          = "ENOSTR"
    | e == eNOSYS          = "ENOSYS"
    | e == eNOTBLK         = "ENOTBLK"
    | e == eNOTCONN        = "ENOTCONN"
    | e == eNOTDIR         = "ENOTDIR"
    | e == eNOTEMPTY       = "ENOTEMPTY"
    | e == eNOTSOCK        = "ENOTSOCK"
    | e == eNOTSUP         = "ENOTSUP"
    | e == eNOTTY          = "ENOTTY"
    | e == eNXIO           = "ENXIO"
    | e == eOPNOTSUPP      = "EOPNOTSUPP"
    | e == ePERM           = "EPERM"
    | e == ePFNOSUPPORT    = "EPFNOSUPPORT"
    | e == ePIPE           = "EPIPE"
    | e == ePROCLIM        = "EPROCLIM"
    | e == ePROCUNAVAIL    = "EPROCUNAVAIL"
    | e == ePROGMISMATCH   = "EPROGMISMATCH"
    | e == ePROGUNAVAIL    = "EPROGUNAVAIL"
    | e == ePROTO          = "EPROTO"
    | e == ePROTONOSUPPORT = "EPROTONOSUPPORT"
    | e == ePROTOTYPE      = "EPROTOTYPE"
    | e == eRANGE          = "ERANGE"
    | e == eREMCHG         = "EREMCHG"
    | e == eREMOTE         = "EREMOTE"
    | e == eROFS           = "EROFS"
    | e == eRPCMISMATCH    = "ERPCMISMATCH"
    | e == eRREMOTE        = "ERREMOTE"
    | e == eSHUTDOWN       = "ESHUTDOWN"
    | e == eSOCKTNOSUPPORT = "ESOCKTNOSUPPORT"
    | e == eSPIPE          = "ESPIPE"
    | e == eSRCH           = "ESRCH"
    | e == eSRMNT          = "ESRMNT"
    | e == eSTALE          = "ESTALE"
    | e == eTIME           = "ETIME"
    | e == eTIMEDOUT       = "ETIMEDOUT"
    | e == eTOOMANYREFS    = "ETOOMANYREFS"
    | e == eTXTBSY         = "ETXTBSY"
    | e == eUSERS          = "EUSERS"
    | e == eWOULDBLOCK     = "EWOULDBLOCK"
    | e == eXDEV           = "EXDEV"
    | otherwise            = show e

throwUnixError :: IOErrno UnixReturn -> IOEInfo -> IO a
throwUnixError e info
    | e == eOK             = throwIO (OtherError              info)
    | e == e2BIG           = throwIO (ResourceExhausted       info)
    | e == eACCES          = throwIO (PermissionDenied        info)
    | e == eADDRINUSE      = throwIO (ResourceBusy            info)
    | e == eADDRNOTAVAIL   = throwIO (UnsupportedOperation    info)
    | e == eADV            = throwIO (OtherError              info)
    | e == eAFNOSUPPORT    = throwIO (UnsupportedOperation    info)
    | e == eAGAIN          = throwIO (ResourceExhausted       info)
    | e == eALREADY        = throwIO (AlreadyExists           info)
    | e == eBADF           = throwIO (InvalidArgument         info)
    | e == eBADMSG         = throwIO (InappropriateType       info)
    | e == eBADRPC         = throwIO (OtherError              info)
    | e == eBUSY           = throwIO (ResourceBusy            info)
    | e == eCHILD          = throwIO (NoSuchThing             info)
    | e == eCOMM           = throwIO (ResourceVanished        info)
    | e == eCONNABORTED    = throwIO (OtherError              info)
    | e == eCONNREFUSED    = throwIO (NoSuchThing             info)
    | e == eCONNRESET      = throwIO (ResourceVanished        info)
    | e == eDEADLK         = throwIO (ResourceBusy            info)
    | e == eDESTADDRREQ    = throwIO (InvalidArgument         info)
    | e == eDIRTY          = throwIO (UnsatisfiedConstraints  info)
    | e == eDOM            = throwIO (InvalidArgument         info)
    | e == eDQUOT          = throwIO (PermissionDenied        info)
    | e == eEXIST          = throwIO (AlreadyExists           info)
    | e == eFAULT          = throwIO (OtherError              info)
    | e == eFBIG           = throwIO (PermissionDenied        info)
    | e == eFTYPE          = throwIO (InappropriateType       info)
    | e == eHOSTDOWN       = throwIO (NoSuchThing             info)
    | e == eHOSTUNREACH    = throwIO (NoSuchThing             info)
    | e == eIDRM           = throwIO (ResourceVanished        info)
    | e == eILSEQ          = throwIO (InvalidArgument         info)
    | e == eINPROGRESS     = throwIO (AlreadyExists           info)
    | e == eINTR           = throwIO (Interrupted             info)
    | e == eINVAL          = throwIO (InvalidArgument         info)
    | e == eIO             = throwIO (HardwareFault           info)
    | e == eISCONN         = throwIO (AlreadyExists           info)
    | e == eISDIR          = throwIO (InappropriateType       info)
    | e == eLOOP           = throwIO (InvalidArgument         info)
    | e == eMFILE          = throwIO (ResourceExhausted       info)
    | e == eMLINK          = throwIO (ResourceExhausted       info)
    | e == eMSGSIZE        = throwIO (ResourceExhausted       info)
    | e == eMULTIHOP       = throwIO (UnsupportedOperation    info)
    | e == eNAMETOOLONG    = throwIO (InvalidArgument         info)
    | e == eNETDOWN        = throwIO (ResourceVanished        info)
    | e == eNETRESET       = throwIO (ResourceVanished        info)
    | e == eNETUNREACH     = throwIO (NoSuchThing             info)
    | e == eNFILE          = throwIO (ResourceExhausted       info)
    | e == eNOBUFS         = throwIO (ResourceExhausted       info)
    | e == eNODATA         = throwIO (NoSuchThing             info)
    | e == eNODEV          = throwIO (UnsupportedOperation    info)
    | e == eNOENT          = throwIO (NoSuchThing             info)
    | e == eNOEXEC         = throwIO (InvalidArgument         info)
    | e == eNOLCK          = throwIO (ResourceExhausted       info)
    | e == eNOLINK         = throwIO (ResourceVanished        info)
    | e == eNOMEM          = throwIO (ResourceExhausted       info)
    | e == eNOMSG          = throwIO (NoSuchThing             info)
    | e == eNONET          = throwIO (NoSuchThing             info)
    | e == eNOPROTOOPT     = throwIO (UnsupportedOperation    info)
    | e == eNOSPC          = throwIO (ResourceExhausted       info)
    | e == eNOSR           = throwIO (ResourceExhausted       info)
    | e == eNOSTR          = throwIO (InvalidArgument         info)
    | e == eNOSYS          = throwIO (UnsupportedOperation    info)
    | e == eNOTBLK         = throwIO (InvalidArgument         info)
    | e == eNOTCONN        = throwIO (InvalidArgument         info)
    | e == eNOTDIR         = throwIO (InappropriateType       info)
    | e == eNOTEMPTY       = throwIO (UnsatisfiedConstraints  info)
    | e == eNOTSOCK        = throwIO (InvalidArgument         info)
    | e == eNOTTY          = throwIO (IllegalOperation        info)
    | e == eNXIO           = throwIO (NoSuchThing             info)
    | e == eOPNOTSUPP      = throwIO (UnsupportedOperation    info)
    | e == ePERM           = throwIO (PermissionDenied        info)
    | e == ePFNOSUPPORT    = throwIO (UnsupportedOperation    info)
    | e == ePIPE           = throwIO (ResourceVanished        info)
    | e == ePROCLIM        = throwIO (PermissionDenied        info)
    | e == ePROCUNAVAIL    = throwIO (UnsupportedOperation    info)
    | e == ePROGMISMATCH   = throwIO (ProtocolError           info)
    | e == ePROGUNAVAIL    = throwIO (UnsupportedOperation    info)
    | e == ePROTO          = throwIO (ProtocolError           info)
    | e == ePROTONOSUPPORT = throwIO (ProtocolError           info)
    | e == ePROTOTYPE      = throwIO (ProtocolError           info)
    | e == eRANGE          = throwIO (UnsupportedOperation    info)
    | e == eREMCHG         = throwIO (ResourceVanished        info)
    | e == eREMOTE         = throwIO (IllegalOperation        info)
    | e == eROFS           = throwIO (PermissionDenied        info)
    | e == eRPCMISMATCH    = throwIO (ProtocolError           info)
    | e == eRREMOTE        = throwIO (IllegalOperation        info)
    | e == eSHUTDOWN       = throwIO (IllegalOperation        info)
    | e == eSOCKTNOSUPPORT = throwIO (UnsupportedOperation    info)
    | e == eSPIPE          = throwIO (UnsupportedOperation    info)
    | e == eSRCH           = throwIO (NoSuchThing             info)
    | e == eSRMNT          = throwIO (UnsatisfiedConstraints  info)
    | e == eSTALE          = throwIO (ResourceVanished        info)
    | e == eTIME           = throwIO (TimeExpired             info)
    | e == eTIMEDOUT       = throwIO (TimeExpired             info)
    | e == eTOOMANYREFS    = throwIO (ResourceExhausted       info)
    | e == eTXTBSY         = throwIO (ResourceBusy            info)
    | e == eUSERS          = throwIO (ResourceExhausted       info)
    | e == eWOULDBLOCK     = throwIO (OtherError              info)
    | e == eXDEV           = throwIO (UnsupportedOperation    info)
    | otherwise            = throwIO (OtherError              info)

--------------------------------------------------------------------------------

-- common unix errno symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN,
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED,
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT,
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ,
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK,
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH,
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK,
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS,
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTSUP, eNOTTY, eNXIO,
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL,
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE,
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN,
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT,
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV                    :: IOErrno UnixReturn
--
-- the cCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure
--
eOK             = UnixErrno 0
e2BIG           = UnixErrno (CONST_E2BIG)
eACCES          = UnixErrno (CONST_EACCES)
eADDRINUSE      = UnixErrno (CONST_EADDRINUSE)
eADDRNOTAVAIL   = UnixErrno (CONST_EADDRNOTAVAIL)
eADV            = UnixErrno (CONST_EADV)
eAFNOSUPPORT    = UnixErrno (CONST_EAFNOSUPPORT)
eAGAIN          = UnixErrno (CONST_EAGAIN)
eALREADY        = UnixErrno (CONST_EALREADY)
eBADF           = UnixErrno (CONST_EBADF)
eBADMSG         = UnixErrno (CONST_EBADMSG)
eBADRPC         = UnixErrno (CONST_EBADRPC)
eBUSY           = UnixErrno (CONST_EBUSY)
eCHILD          = UnixErrno (CONST_ECHILD)
eCOMM           = UnixErrno (CONST_ECOMM)
eCONNABORTED    = UnixErrno (CONST_ECONNABORTED)
eCONNREFUSED    = UnixErrno (CONST_ECONNREFUSED)
eCONNRESET      = UnixErrno (CONST_ECONNRESET)
eDEADLK         = UnixErrno (CONST_EDEADLK)
eDESTADDRREQ    = UnixErrno (CONST_EDESTADDRREQ)
eDIRTY          = UnixErrno (CONST_EDIRTY)
eDOM            = UnixErrno (CONST_EDOM)
eDQUOT          = UnixErrno (CONST_EDQUOT)
eEXIST          = UnixErrno (CONST_EEXIST)
eFAULT          = UnixErrno (CONST_EFAULT)
eFBIG           = UnixErrno (CONST_EFBIG)
eFTYPE          = UnixErrno (CONST_EFTYPE)
eHOSTDOWN       = UnixErrno (CONST_EHOSTDOWN)
eHOSTUNREACH    = UnixErrno (CONST_EHOSTUNREACH)
eIDRM           = UnixErrno (CONST_EIDRM)
eILSEQ          = UnixErrno (CONST_EILSEQ)
eINPROGRESS     = UnixErrno (CONST_EINPROGRESS)
eINTR           = UnixErrno (CONST_EINTR)
eINVAL          = UnixErrno (CONST_EINVAL)
eIO             = UnixErrno (CONST_EIO)
eISCONN         = UnixErrno (CONST_EISCONN)
eISDIR          = UnixErrno (CONST_EISDIR)
eLOOP           = UnixErrno (CONST_ELOOP)
eMFILE          = UnixErrno (CONST_EMFILE)
eMLINK          = UnixErrno (CONST_EMLINK)
eMSGSIZE        = UnixErrno (CONST_EMSGSIZE)
eMULTIHOP       = UnixErrno (CONST_EMULTIHOP)
eNAMETOOLONG    = UnixErrno (CONST_ENAMETOOLONG)
eNETDOWN        = UnixErrno (CONST_ENETDOWN)
eNETRESET       = UnixErrno (CONST_ENETRESET)
eNETUNREACH     = UnixErrno (CONST_ENETUNREACH)
eNFILE          = UnixErrno (CONST_ENFILE)
eNOBUFS         = UnixErrno (CONST_ENOBUFS)
eNODATA         = UnixErrno (CONST_ENODATA)
eNODEV          = UnixErrno (CONST_ENODEV)
eNOENT          = UnixErrno (CONST_ENOENT)
eNOEXEC         = UnixErrno (CONST_ENOEXEC)
eNOLCK          = UnixErrno (CONST_ENOLCK)
eNOLINK         = UnixErrno (CONST_ENOLINK)
eNOMEM          = UnixErrno (CONST_ENOMEM)
eNOMSG          = UnixErrno (CONST_ENOMSG)
eNONET          = UnixErrno (CONST_ENONET)
eNOPROTOOPT     = UnixErrno (CONST_ENOPROTOOPT)
eNOSPC          = UnixErrno (CONST_ENOSPC)
eNOSR           = UnixErrno (CONST_ENOSR)
eNOSTR          = UnixErrno (CONST_ENOSTR)
eNOSYS          = UnixErrno (CONST_ENOSYS)
eNOTBLK         = UnixErrno (CONST_ENOTBLK)
eNOTCONN        = UnixErrno (CONST_ENOTCONN)
eNOTDIR         = UnixErrno (CONST_ENOTDIR)
eNOTEMPTY       = UnixErrno (CONST_ENOTEMPTY)
eNOTSOCK        = UnixErrno (CONST_ENOTSOCK)
eNOTSUP         = UnixErrno (CONST_ENOTSUP)
eNOTTY          = UnixErrno (CONST_ENOTTY)
eNXIO           = UnixErrno (CONST_ENXIO)
eOPNOTSUPP      = UnixErrno (CONST_EOPNOTSUPP)
ePERM           = UnixErrno (CONST_EPERM)
ePFNOSUPPORT    = UnixErrno (CONST_EPFNOSUPPORT)
ePIPE           = UnixErrno (CONST_EPIPE)
ePROCLIM        = UnixErrno (CONST_EPROCLIM)
ePROCUNAVAIL    = UnixErrno (CONST_EPROCUNAVAIL)
ePROGMISMATCH   = UnixErrno (CONST_EPROGMISMATCH)
ePROGUNAVAIL    = UnixErrno (CONST_EPROGUNAVAIL)
ePROTO          = UnixErrno (CONST_EPROTO)
ePROTONOSUPPORT = UnixErrno (CONST_EPROTONOSUPPORT)
ePROTOTYPE      = UnixErrno (CONST_EPROTOTYPE)
eRANGE          = UnixErrno (CONST_ERANGE)
eREMCHG         = UnixErrno (CONST_EREMCHG)
eREMOTE         = UnixErrno (CONST_EREMOTE)
eROFS           = UnixErrno (CONST_EROFS)
eRPCMISMATCH    = UnixErrno (CONST_ERPCMISMATCH)
eRREMOTE        = UnixErrno (CONST_ERREMOTE)
eSHUTDOWN       = UnixErrno (CONST_ESHUTDOWN)
eSOCKTNOSUPPORT = UnixErrno (CONST_ESOCKTNOSUPPORT)
eSPIPE          = UnixErrno (CONST_ESPIPE)
eSRCH           = UnixErrno (CONST_ESRCH)
eSRMNT          = UnixErrno (CONST_ESRMNT)
eSTALE          = UnixErrno (CONST_ESTALE)
eTIME           = UnixErrno (CONST_ETIME)
eTIMEDOUT       = UnixErrno (CONST_ETIMEDOUT)
eTOOMANYREFS    = UnixErrno (CONST_ETOOMANYREFS)
eTXTBSY         = UnixErrno (CONST_ETXTBSY)
eUSERS          = UnixErrno (CONST_EUSERS)
eWOULDBLOCK     = UnixErrno (CONST_EWOULDBLOCK)
eXDEV           = UnixErrno (CONST_EXDEV)
