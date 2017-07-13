{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}

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
do some extra work to keep error message's quality. New defined I/O exceptions are also encouraged to include a 'CallStack',
since it helps a lot when debugging.

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
  , throwErrno
  , throwErrorIf
  , throwErrnoIfMinus1
  , throwErrnoIfNull
  , throwErrnoIfRetry
  , throwErrnoIfMinus1Retry
  , throwErrnoIfNullRetry
  , throwErrnoIfRetryMayBlock
  , throwErrnoIfMinus1RetryMayBlock
  , throwErrnoIfNullRetryMayBlock
    -- * Errno type
  , Errno(..)
  , isValidErrno
  , getErrno
  , resetErrno
  , showErrno
  ) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import GHC.Stack

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

-- | Throw io exception corresponding to the current value of 'getErrno'.
--
-- The mapping between errno and exception type are model after "Foreign.C.Error", if there's missing
-- or wrong mapping, please report.
--
throwErrno :: CallStack -- callstack
                 -> String    -- device info, such as filename, socket address, etc
                 -> IO a
throwErrno cstack dev = do
    errno <- getErrno
    desc <- (strerror errno >>= peekCString)
    let info = IOEInfo errno desc dev cstack
    case () of
        _

            | errno == eOK             -> throwIO (OtherError              info)
            | errno == e2BIG           -> throwIO (ResourceExhausted       info)
            | errno == eACCES          -> throwIO (PermissionDenied        info)
            | errno == eADDRINUSE      -> throwIO (ResourceBusy            info)
            | errno == eADDRNOTAVAIL   -> throwIO (UnsupportedOperation    info)
            | errno == eADV            -> throwIO (OtherError              info)
            | errno == eAFNOSUPPORT    -> throwIO (UnsupportedOperation    info)
            | errno == eAGAIN          -> throwIO (ResourceExhausted       info)
            | errno == eALREADY        -> throwIO (AlreadyExists           info)
            | errno == eBADF           -> throwIO (InvalidArgument         info)
            | errno == eBADMSG         -> throwIO (InappropriateType       info)
            | errno == eBADRPC         -> throwIO (OtherError              info)
            | errno == eBUSY           -> throwIO (ResourceBusy            info)
            | errno == eCHILD          -> throwIO (NoSuchThing             info)
            | errno == eCOMM           -> throwIO (ResourceVanished        info)
            | errno == eCONNABORTED    -> throwIO (OtherError              info)
            | errno == eCONNREFUSED    -> throwIO (NoSuchThing             info)
            | errno == eCONNRESET      -> throwIO (ResourceVanished        info)
            | errno == eDEADLK         -> throwIO (ResourceBusy            info)
            | errno == eDESTADDRREQ    -> throwIO (InvalidArgument         info)
            | errno == eDIRTY          -> throwIO (UnsatisfiedConstraints  info)
            | errno == eDOM            -> throwIO (InvalidArgument         info)
            | errno == eDQUOT          -> throwIO (PermissionDenied        info)
            | errno == eEXIST          -> throwIO (AlreadyExists           info)
            | errno == eFAULT          -> throwIO (OtherError              info)
            | errno == eFBIG           -> throwIO (PermissionDenied        info)
            | errno == eFTYPE          -> throwIO (InappropriateType       info)
            | errno == eHOSTDOWN       -> throwIO (NoSuchThing             info)
            | errno == eHOSTUNREACH    -> throwIO (NoSuchThing             info)
            | errno == eIDRM           -> throwIO (ResourceVanished        info)
            | errno == eILSEQ          -> throwIO (InvalidArgument         info)
            | errno == eINPROGRESS     -> throwIO (AlreadyExists           info)
            | errno == eINTR           -> throwIO (Interrupted             info)
            | errno == eINVAL          -> throwIO (InvalidArgument         info)
            | errno == eIO             -> throwIO (HardwareFault           info)
            | errno == eISCONN         -> throwIO (AlreadyExists           info)
            | errno == eISDIR          -> throwIO (InappropriateType       info)
            | errno == eLOOP           -> throwIO (InvalidArgument         info)
            | errno == eMFILE          -> throwIO (ResourceExhausted       info)
            | errno == eMLINK          -> throwIO (ResourceExhausted       info)
            | errno == eMSGSIZE        -> throwIO (ResourceExhausted       info)
            | errno == eMULTIHOP       -> throwIO (UnsupportedOperation    info)
            | errno == eNAMETOOLONG    -> throwIO (InvalidArgument         info)
            | errno == eNETDOWN        -> throwIO (ResourceVanished        info)
            | errno == eNETRESET       -> throwIO (ResourceVanished        info)
            | errno == eNETUNREACH     -> throwIO (NoSuchThing             info)
            | errno == eNFILE          -> throwIO (ResourceExhausted       info)
            | errno == eNOBUFS         -> throwIO (ResourceExhausted       info)
            | errno == eNODATA         -> throwIO (NoSuchThing             info)
            | errno == eNODEV          -> throwIO (UnsupportedOperation    info)
            | errno == eNOENT          -> throwIO (NoSuchThing             info)
            | errno == eNOEXEC         -> throwIO (InvalidArgument         info)
            | errno == eNOLCK          -> throwIO (ResourceExhausted       info)
            | errno == eNOLINK         -> throwIO (ResourceVanished        info)
            | errno == eNOMEM          -> throwIO (ResourceExhausted       info)
            | errno == eNOMSG          -> throwIO (NoSuchThing             info)
            | errno == eNONET          -> throwIO (NoSuchThing             info)
            | errno == eNOPROTOOPT     -> throwIO (UnsupportedOperation    info)
            | errno == eNOSPC          -> throwIO (ResourceExhausted       info)
            | errno == eNOSR           -> throwIO (ResourceExhausted       info)
            | errno == eNOSTR          -> throwIO (InvalidArgument         info)
            | errno == eNOSYS          -> throwIO (UnsupportedOperation    info)
            | errno == eNOTBLK         -> throwIO (InvalidArgument         info)
            | errno == eNOTCONN        -> throwIO (InvalidArgument         info)
            | errno == eNOTDIR         -> throwIO (InappropriateType       info)
            | errno == eNOTEMPTY       -> throwIO (UnsatisfiedConstraints  info)
            | errno == eNOTSOCK        -> throwIO (InvalidArgument         info)
            | errno == eNOTTY          -> throwIO (IllegalOperation        info)
            | errno == eNXIO           -> throwIO (NoSuchThing             info)
            | errno == eOPNOTSUPP      -> throwIO (UnsupportedOperation    info)
            | errno == ePERM           -> throwIO (PermissionDenied        info)
            | errno == ePFNOSUPPORT    -> throwIO (UnsupportedOperation    info)
            | errno == ePIPE           -> throwIO (ResourceVanished        info)
            | errno == ePROCLIM        -> throwIO (PermissionDenied        info)
            | errno == ePROCUNAVAIL    -> throwIO (UnsupportedOperation    info)
            | errno == ePROGMISMATCH   -> throwIO (ProtocolError           info)
            | errno == ePROGUNAVAIL    -> throwIO (UnsupportedOperation    info)
            | errno == ePROTO          -> throwIO (ProtocolError           info)
            | errno == ePROTONOSUPPORT -> throwIO (ProtocolError           info)
            | errno == ePROTOTYPE      -> throwIO (ProtocolError           info)
            | errno == eRANGE          -> throwIO (UnsupportedOperation    info)
            | errno == eREMCHG         -> throwIO (ResourceVanished        info)
            | errno == eREMOTE         -> throwIO (IllegalOperation        info)
            | errno == eROFS           -> throwIO (PermissionDenied        info)
            | errno == eRPCMISMATCH    -> throwIO (ProtocolError           info)
            | errno == eRREMOTE        -> throwIO (IllegalOperation        info)
            | errno == eSHUTDOWN       -> throwIO (IllegalOperation        info)
            | errno == eSOCKTNOSUPPORT -> throwIO (UnsupportedOperation    info)
            | errno == eSPIPE          -> throwIO (UnsupportedOperation    info)
            | errno == eSRCH           -> throwIO (NoSuchThing             info)
            | errno == eSRMNT          -> throwIO (UnsatisfiedConstraints  info)
            | errno == eSTALE          -> throwIO (ResourceVanished        info)
            | errno == eTIME           -> throwIO (TimeExpired             info)
            | errno == eTIMEDOUT       -> throwIO (TimeExpired             info)
            | errno == eTOOMANYREFS    -> throwIO (ResourceExhausted       info)
            | errno == eTXTBSY         -> throwIO (ResourceBusy            info)
            | errno == eUSERS          -> throwIO (ResourceExhausted       info)
            | errno == eWOULDBLOCK     -> throwIO (OtherError              info)
            | errno == eXDEV           -> throwIO (UnsupportedOperation    info)
            | otherwise                -> throwIO (OtherError              info)


--------------------------------------------------------------------------------

-- | Throw io exception based on given predicate.
--
throwErrorIf :: (a -> Bool) -- ^ predicate to apply to the result value of the IO operation,
                            -- we will call 'throwErrno' on True
             -> CallStack   -- ^ callstack
             -> String      -- ^ device information
             -> IO a        -- ^ the IO operation to be performed
             -> IO a
throwErrorIf pred cstack dev f = do
    res <- f
    if pred res then throwErrno cstack dev else return res

-- | as 'throwErrorIf', but retry the 'IO' action when it yields the
-- error code 'eINTR' - this amounts to the standard retry loop for
-- interrupted POSIX system calls.
--
throwErrnoIfRetry :: (a -> Bool) -> CallStack -> String -> IO a -> IO a
throwErrnoIfRetry pred cstack dev f  = do
    res <- f
    if pred res
    then do
        err <- getErrno
        if err == eINTR
        then throwErrnoIfRetry pred cstack dev f
        else throwErrno cstack dev
    else return res

-- | as 'throwErrnoIfRetry', but additionally if the operation
-- yields the error code 'eAGAIN' or 'eWOULDBLOCK', an alternative
-- action is performed before retrying.
--
throwErrnoIfRetryMayBlock :: (a -> Bool)  -- ^ predicate to apply to the result value
                                   -- of the 'IO' operation
                   -> CallStack    -- ^ callstack
                   -> String       -- ^ device info
                   -> IO a         -- ^ the 'IO' operation to be performed
                   -> IO b         -- ^ action to execute before retrying if
                                   -- an immediate retry would block
                   -> IO a
throwErrnoIfRetryMayBlock pred cstack dev f on_block  = do
    res <- f
    if pred res
    then do
        err <- getErrno
        if err == eINTR
        then throwErrnoIfRetryMayBlock pred cstack dev f on_block
        else if err == eWOULDBLOCK || err == eAGAIN
             then do _ <- on_block
                     throwErrnoIfRetryMayBlock pred cstack dev f on_block
             else throwErrno cstack dev
    else return res

-- | Throw io exception corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@.
--
throwErrnoIfMinus1 :: (Eq a, Num a) => CallStack -> String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrorIf (== -1)

-- | Throw io exception corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@, but retries in case of
-- an interrupted operation.
--
throwErrnoIfMinus1Retry :: (Eq a, Num a) => CallStack -> String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

-- | as 'throwErrnoIfMinus1Retry', but checks for operations that would block.
--
throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a) => CallStack -> String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock = throwErrnoIfRetryMayBlock (== -1)

-- | Throw io exception corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr'.
--
throwErrnoIfNull :: CallStack -> String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrorIf (== nullPtr)

-- | Throw io exception corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr',
-- but retry in case of an interrupted operation.
--
throwErrnoIfNullRetry :: CallStack -> String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry = throwErrnoIfRetry (== nullPtr)

-- | as 'throwErrnoIfNullRetry', but checks for operations that would block.
--
throwErrnoIfNullRetryMayBlock :: CallStack -> String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock = throwErrnoIfRetryMayBlock (== nullPtr)

--------------------------------------------------------------------------------

-- | IO exceptions informations.
--
data IOEInfo = IOEInfo
    { ioeErrno :: Errno           -- ^ the errno
    , ioeDescription :: String    -- ^ description from strerror
    , ioeDevice :: String         -- ^ device info, such as filename, socket address, etc
    , ioeCallStack :: CallStack   -- ^ lightweight partial call-stack
    }

instance Show IOEInfo where
    show (IOEInfo errno desc dev cstack) =
         "{errno:" ++ (showErrno errno) ++ ", " ++
         "description:" ++ desc ++ ", " ++
         "device:" ++ dev ++ ", " ++
         "callstack:" ++ prettyCallStack cstack ++ "}"

showErrno :: Errno -> String
showErrno e
    | e == eOK             = "EOK"
    | e == e2BIG           = "E2BIG"
    | e == eACCES          = "EACCES"
    | e == eADDRINUSE      = "EADDRINUSE"
    | e == eADDRNOTAVAIL   = "EADDRNOTAVAIL"
    | e == eADV            = "EADV"
    | e == eAFNOSUPPORT    = "EAFNOSUPPORT"
    | e == eAGAIN          = "EAGAIN"
    | e == eALREADY        = "EALREADY"
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

foreign import ccall unsafe "string.h" strerror :: Errno -> IO (Ptr CChar)
