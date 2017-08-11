{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.UV.Exception where

#include "uv.h"

import Foreign.C
import GHC.Stack.Compat
import System.IO.Exception
import Data.Bits
import Foreign.Storable

newtype UVReturn a = UVReturn a
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

instance IOReturn UVReturn where
    newtype IOErrno UVReturn = UVErrno CInt
        deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)
    isError (UVReturn r) = r < 0
    isInterrupt e = e == uV_EINTR
    isBlock e = e == uV_EAGAIN
    getReturn (UVReturn r) = r
    getErrno (UVReturn r) = return (UVErrno (fromIntegral r))
    nameErrno e = uv_err_name e >>= peekCString
    descErrno e = uv_strerror e >>= peekCString
    throwErrno = throwUVError

foreign import ccall unsafe uv_strerror :: IOErrno UVReturn -> IO CString
foreign import ccall unsafe uv_err_name :: IOErrno UVReturn -> IO CString

--------------------------------------------------------------------------------

throwUVError :: HasCallStack => IOErrno UVReturn -> String -> IO a
throwUVError e dev = do
        name <- nameErrno e
        desc <- descErrno e
        let info = IOEInfo name desc dev callStack
        case () of
            _
                | e == uV_EOF             -> throwIO (EOF                     info)   -- usually you want to deal with it differently
                | e == uV_E2BIG           -> throwIO (ResourceExhausted       info)
                | e == uV_EACCES          -> throwIO (PermissionDenied        info)
                | e == uV_EADDRINUSE      -> throwIO (ResourceBusy            info)
                | e == uV_EADDRNOTAVAIL   -> throwIO (UnsupportedOperation    info)
                | e == uV_EAFNOSUPPORT    -> throwIO (UnsupportedOperation    info)
                | e == uV_EAGAIN          -> throwIO (ResourceExhausted       info)
                | e == uV_EAI_ADDRFAMILY  -> throwIO (UnsupportedOperation    info)
                | e == uV_EAI_AGAIN       -> throwIO (ResourceExhausted       info)
                | e == uV_EAI_BADFLAGS    -> throwIO (UnsupportedOperation    info)
                | e == uV_EAI_BADHINTS    -> throwIO (UnsupportedOperation    info)
                | e == uV_EAI_CANCELED    -> throwIO (ResourceVanished        info)
                | e == uV_EAI_FAIL        -> throwIO (OtherError              info)
                | e == uV_EAI_FAMILY      -> throwIO (UnsupportedOperation    info)
                | e == uV_EAI_MEMORY      -> throwIO (ResourceExhausted       info)
                | e == uV_EAI_NODATA      -> throwIO (NoSuchThing             info)
                | e == uV_EAI_NONAME      -> throwIO (NoSuchThing             info)
                | e == uV_EAI_OVERFLOW    -> throwIO (InvalidArgument         info)
                | e == uV_EAI_PROTOCOL    -> throwIO (ProtocolError           info)
                | e == uV_EAI_SERVICE     -> throwIO (UnsupportedOperation    info)
                | e == uV_EAI_SOCKTYPE    -> throwIO (UnsupportedOperation    info)
                | e == uV_EALREADY        -> throwIO (AlreadyExists           info)
                | e == uV_EBADF           -> throwIO (InvalidArgument         info)
                | e == uV_EBUSY           -> throwIO (ResourceBusy            info)
                | e == uV_ECANCELED       -> throwIO (ResourceVanished        info)
                | e == uV_ECHARSET        -> throwIO (OtherError              info)
                | e == uV_ECONNABORTED    -> throwIO (ResourceVanished        info)
                | e == uV_ECONNREFUSED    -> throwIO (NoSuchThing             info)
                | e == uV_ECONNRESET      -> throwIO (ResourceVanished        info)
                | e == uV_EDESTADDRREQ    -> throwIO (InvalidArgument         info)
                | e == uV_EEXIST          -> throwIO (AlreadyExists           info)
                | e == uV_EFAULT          -> throwIO (OtherError              info)
                | e == uV_EFBIG           -> throwIO (PermissionDenied        info)
                | e == uV_EHOSTUNREACH    -> throwIO (NoSuchThing             info)
                | e == uV_EINTR           -> throwIO (Interrupted             info)
                | e == uV_EINVAL          -> throwIO (InvalidArgument         info)
                | e == uV_EIO             -> throwIO (HardwareFault           info)
                | e == uV_EISCONN         -> throwIO (AlreadyExists           info)
                | e == uV_EISDIR          -> throwIO (InappropriateType       info)
                | e == uV_ELOOP           -> throwIO (InvalidArgument         info)
                | e == uV_EMFILE          -> throwIO (ResourceExhausted       info)
                | e == uV_EMSGSIZE        -> throwIO (ResourceExhausted       info)
                | e == uV_ENAMETOOLONG    -> throwIO (InvalidArgument         info)
                | e == uV_ENETDOWN        -> throwIO (ResourceVanished        info)
                | e == uV_ENETUNREACH     -> throwIO (NoSuchThing             info)
                | e == uV_ENFILE          -> throwIO (ResourceExhausted       info)
                | e == uV_ENOBUFS         -> throwIO (ResourceExhausted       info)
                | e == uV_ENODEV          -> throwIO (UnsupportedOperation    info)
                | e == uV_ENOENT          -> throwIO (NoSuchThing             info)
                | e == uV_ENOMEM          -> throwIO (ResourceExhausted       info)
                | e == uV_ENOPROTOOPT     -> throwIO (UnsupportedOperation    info)
                | e == uV_ENOSPC          -> throwIO (ResourceExhausted       info)
                | e == uV_ENOSYS          -> throwIO (UnsupportedOperation    info)
                | e == uV_ENOTCONN        -> throwIO (InvalidArgument         info)
                | e == uV_ENOTDIR         -> throwIO (InappropriateType       info)
                | e == uV_ENOTEMPTY       -> throwIO (UnsatisfiedConstraints  info)
                | e == uV_ENOTSOCK        -> throwIO (InvalidArgument         info)
                | e == uV_ENOTSUP         -> throwIO (UnsupportedOperation    info)
                | e == uV_EPERM           -> throwIO (PermissionDenied        info)
                | e == uV_EPIPE           -> throwIO (ResourceVanished        info)
                | e == uV_EPROTO          -> throwIO (ProtocolError           info)
                | e == uV_EPROTONOSUPPORT -> throwIO (ProtocolError           info)
                | e == uV_EPROTOTYPE      -> throwIO (ProtocolError           info)
                | e == uV_ERANGE          -> throwIO (UnsupportedOperation    info)
                | e == uV_EROFS           -> throwIO (PermissionDenied        info)
                | e == uV_ESHUTDOWN       -> throwIO (IllegalOperation        info)
                | e == uV_ESPIPE          -> throwIO (UnsupportedOperation    info)
                | e == uV_ESRCH           -> throwIO (NoSuchThing             info)
                | e == uV_ETIMEDOUT       -> throwIO (TimeExpired             info)
                | e == uV_ETXTBSY         -> throwIO (ResourceBusy            info)
                | e == uV_EXDEV           -> throwIO (UnsupportedOperation    info)
                | e == uV_UNKNOWN         -> throwIO (OtherError              info)
                | e == uV_ENXIO           -> throwIO (NoSuchThing             info)
                | e == uV_EMLINK          -> throwIO (ResourceExhausted       info)
                | otherwise               -> throwIO (OtherError              info)

--------------------------------------------------------------------------------

-- | argument list too long                         
#{enum IOErrno UVReturn, UVErrno, uV_E2BIG          = UV_E2BIG          } 
-- | permission denied                              
#{enum IOErrno UVReturn, UVErrno, uV_EACCES         = UV_EACCES         } 
-- | address already in use                         
#{enum IOErrno UVReturn, UVErrno, uV_EADDRINUSE     = UV_EADDRINUSE     } 
-- | address not available                          
#{enum IOErrno UVReturn, UVErrno, uV_EADDRNOTAVAIL  = UV_EADDRNOTAVAIL  } 
-- | address family not supported                   
#{enum IOErrno UVReturn, UVErrno, uV_EAFNOSUPPORT   = UV_EAFNOSUPPORT   } 
-- | resource temporarily unavailable               
#{enum IOErrno UVReturn, UVErrno, uV_EAGAIN         = UV_EAGAIN         } 
-- | address family not supported                   
#{enum IOErrno UVReturn, UVErrno, uV_EAI_ADDRFAMILY = UV_EAI_ADDRFAMILY } 
-- | temporary failure                              
#{enum IOErrno UVReturn, UVErrno, uV_EAI_AGAIN      = UV_EAI_AGAIN      } 
-- | bad ai_flags value                             
#{enum IOErrno UVReturn, UVErrno, uV_EAI_BADFLAGS   = UV_EAI_BADFLAGS   } 
-- | invalid value for hints                        
#{enum IOErrno UVReturn, UVErrno, uV_EAI_BADHINTS   = UV_EAI_BADHINTS   } 
-- | request canceled                               
#{enum IOErrno UVReturn, UVErrno, uV_EAI_CANCELED   = UV_EAI_CANCELED   } 
-- | permanent failure                              
#{enum IOErrno UVReturn, UVErrno, uV_EAI_FAIL       = UV_EAI_FAIL       } 
-- | ai_family not supported                        
#{enum IOErrno UVReturn, UVErrno, uV_EAI_FAMILY     = UV_EAI_FAMILY     } 
-- | out of memory                                  
#{enum IOErrno UVReturn, UVErrno, uV_EAI_MEMORY     = UV_EAI_MEMORY     } 
-- | no address                                     
#{enum IOErrno UVReturn, UVErrno, uV_EAI_NODATA     = UV_EAI_NODATA     } 
-- | unknown node or service                        
#{enum IOErrno UVReturn, UVErrno, uV_EAI_NONAME     = UV_EAI_NONAME     } 
-- | argument buffer overflow                       
#{enum IOErrno UVReturn, UVErrno, uV_EAI_OVERFLOW   = UV_EAI_OVERFLOW   } 
-- | resolved protocol is unknown                   
#{enum IOErrno UVReturn, UVErrno, uV_EAI_PROTOCOL   = UV_EAI_PROTOCOL   } 
-- | service not available for socket type          
#{enum IOErrno UVReturn, UVErrno, uV_EAI_SERVICE    = UV_EAI_SERVICE    } 
-- | socket type not supported                      
#{enum IOErrno UVReturn, UVErrno, uV_EAI_SOCKTYPE   = UV_EAI_SOCKTYPE   } 
-- | connection already in progress                 
#{enum IOErrno UVReturn, UVErrno, uV_EALREADY       = UV_EALREADY       } 
-- | bad file descriptor                            
#{enum IOErrno UVReturn, UVErrno, uV_EBADF          = UV_EBADF          } 
-- | resource busy or locked                        
#{enum IOErrno UVReturn, UVErrno, uV_EBUSY          = UV_EBUSY          } 
-- | operation canceled                             
#{enum IOErrno UVReturn, UVErrno, uV_ECANCELED      = UV_ECANCELED      } 
-- | invalid Unicode character                      
#{enum IOErrno UVReturn, UVErrno, uV_ECHARSET       = UV_ECHARSET       } 
-- | software caused connection abort               
#{enum IOErrno UVReturn, UVErrno, uV_ECONNABORTED   = UV_ECONNABORTED   } 
-- | connection refused                             
#{enum IOErrno UVReturn, UVErrno, uV_ECONNREFUSED   = UV_ECONNREFUSED   } 
-- | connection reset by peer                       
#{enum IOErrno UVReturn, UVErrno, uV_ECONNRESET     = UV_ECONNRESET     } 
-- | destination address required                   
#{enum IOErrno UVReturn, UVErrno, uV_EDESTADDRREQ   = UV_EDESTADDRREQ   } 
-- | file already exists                            
#{enum IOErrno UVReturn, UVErrno, uV_EEXIST         = UV_EEXIST         } 
-- | bad address in system call argument            
#{enum IOErrno UVReturn, UVErrno, uV_EFAULT         = UV_EFAULT         } 
-- | file too large                                 
#{enum IOErrno UVReturn, UVErrno, uV_EFBIG          = UV_EFBIG          } 
-- | host is unreachable                            
#{enum IOErrno UVReturn, UVErrno, uV_EHOSTUNREACH   = UV_EHOSTUNREACH   } 
-- | interrupted system call                        
#{enum IOErrno UVReturn, UVErrno, uV_EINTR          = UV_EINTR          } 
-- | invalid argument                               
#{enum IOErrno UVReturn, UVErrno, uV_EINVAL         = UV_EINVAL         } 
-- | i/o error                                      
#{enum IOErrno UVReturn, UVErrno, uV_EIO            = UV_EIO            } 
-- | socket is already connected                    
#{enum IOErrno UVReturn, UVErrno, uV_EISCONN        = UV_EISCONN        } 
-- | illegal operation on a directory               
#{enum IOErrno UVReturn, UVErrno, uV_EISDIR         = UV_EISDIR         } 
-- | too many symbolic links encountered            
#{enum IOErrno UVReturn, UVErrno, uV_ELOOP          = UV_ELOOP          } 
-- | too many open files                            
#{enum IOErrno UVReturn, UVErrno, uV_EMFILE         = UV_EMFILE         } 
-- | message too long                               
#{enum IOErrno UVReturn, UVErrno, uV_EMSGSIZE       = UV_EMSGSIZE       } 
-- | name too long                                  
#{enum IOErrno UVReturn, UVErrno, uV_ENAMETOOLONG   = UV_ENAMETOOLONG   } 
-- | network is down                                
#{enum IOErrno UVReturn, UVErrno, uV_ENETDOWN       = UV_ENETDOWN       } 
-- | network is unreachable                         
#{enum IOErrno UVReturn, UVErrno, uV_ENETUNREACH    = UV_ENETUNREACH    } 
-- | file table overflow                            
#{enum IOErrno UVReturn, UVErrno, uV_ENFILE         = UV_ENFILE         } 
-- | no buffer space available                      
#{enum IOErrno UVReturn, UVErrno, uV_ENOBUFS        = UV_ENOBUFS        } 
-- | no such device                                 
#{enum IOErrno UVReturn, UVErrno, uV_ENODEV         = UV_ENODEV         } 
-- | no such file or directory                      
#{enum IOErrno UVReturn, UVErrno, uV_ENOENT         = UV_ENOENT         } 
-- | not enough memory                              
#{enum IOErrno UVReturn, UVErrno, uV_ENOMEM         = UV_ENOMEM         } 
-- | machine is not on the network                  
#{enum IOErrno UVReturn, UVErrno, uV_ENONET         = UV_ENONET         } 
-- | protocol not available                         
#{enum IOErrno UVReturn, UVErrno, uV_ENOPROTOOPT    = UV_ENOPROTOOPT    } 
-- | no space left on device                        
#{enum IOErrno UVReturn, UVErrno, uV_ENOSPC         = UV_ENOSPC         } 
-- | function not implemented                       
#{enum IOErrno UVReturn, UVErrno, uV_ENOSYS         = UV_ENOSYS         } 
-- | socket is not connected                        
#{enum IOErrno UVReturn, UVErrno, uV_ENOTCONN       = UV_ENOTCONN       } 
-- | not a directory                                
#{enum IOErrno UVReturn, UVErrno, uV_ENOTDIR        = UV_ENOTDIR        } 
-- | directory not empty                            
#{enum IOErrno UVReturn, UVErrno, uV_ENOTEMPTY      = UV_ENOTEMPTY      } 
-- | socket operation on non-socket                 
#{enum IOErrno UVReturn, UVErrno, uV_ENOTSOCK       = UV_ENOTSOCK       } 
-- | operation not supported on socket              
#{enum IOErrno UVReturn, UVErrno, uV_ENOTSUP        = UV_ENOTSUP        } 
-- | operation not permitted                        
#{enum IOErrno UVReturn, UVErrno, uV_EPERM          = UV_EPERM          } 
-- | broken pipe                                    
#{enum IOErrno UVReturn, UVErrno, uV_EPIPE          = UV_EPIPE          } 
-- | protocol error                                 
#{enum IOErrno UVReturn, UVErrno, uV_EPROTO         = UV_EPROTO         } 
-- | protocol not supported                         
#{enum IOErrno UVReturn, UVErrno, uV_EPROTONOSUPPORT= UV_EPROTONOSUPPORT} 
-- | protocol wrong type for socket                 
#{enum IOErrno UVReturn, UVErrno, uV_EPROTOTYPE     = UV_EPROTOTYPE     } 
-- | result too large                               
#{enum IOErrno UVReturn, UVErrno, uV_ERANGE         = UV_ERANGE         } 
-- | read-only file system                          
#{enum IOErrno UVReturn, UVErrno, uV_EROFS          = UV_EROFS          } 
-- | cannot send after transport endpoint shutdown  
#{enum IOErrno UVReturn, UVErrno, uV_ESHUTDOWN      = UV_ESHUTDOWN      } 
-- | invalid seek                                   
#{enum IOErrno UVReturn, UVErrno, uV_ESPIPE         = UV_ESPIPE         } 
-- | no such process                                
#{enum IOErrno UVReturn, UVErrno, uV_ESRCH          = UV_ESRCH          } 
-- | connection timed out                           
#{enum IOErrno UVReturn, UVErrno, uV_ETIMEDOUT      = UV_ETIMEDOUT      } 
-- | text file is busy                              
#{enum IOErrno UVReturn, UVErrno, uV_ETXTBSY        = UV_ETXTBSY        } 
-- | cross-device link not permitted                
#{enum IOErrno UVReturn, UVErrno, uV_EXDEV          = UV_EXDEV          } 
-- | unknown error                                  
#{enum IOErrno UVReturn, UVErrno, uV_UNKNOWN        = UV_UNKNOWN        } 
-- | end of file                                    
#{enum IOErrno UVReturn, UVErrno, uV_EOF            = UV_EOF            } 
-- | no such device or address                      
#{enum IOErrno UVReturn, UVErrno, uV_ENXIO          = UV_ENXIO          } 
-- | too many links                                 
#{enum IOErrno UVReturn, UVErrno, uV_EMLINK         = UV_EMLINK         }       

