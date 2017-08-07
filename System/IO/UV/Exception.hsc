module System.IO.UV.Exception (throwUVErrorIfMinus) where

import Foreign.C
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Stack.Compat
import System.IO.Exception
import System.IO.UV.Base

#include "uv.h"

newtype UVErrno = UVErrno CInt deriving (Eq, Ord)

instance IOErrno UVErrno where
    showErrno (UVErrno errno) = unsafeDupablePerformIO $ uv_err_name errno >>= peekCString
    fromErrnoValue v = UVErrno v
    toErrnoValue (UVErrno v) = v

-- | argument list too long                         
#{enum UVErrno, UVErrno, uV_E2BIG          = UV_E2BIG          } 
-- | permission denied                              
#{enum UVErrno, UVErrno, uV_EACCES         = UV_EACCES         } 
-- | address already in use                         
#{enum UVErrno, UVErrno, uV_EADDRINUSE     = UV_EADDRINUSE     } 
-- | address not available                          
#{enum UVErrno, UVErrno, uV_EADDRNOTAVAIL  = UV_EADDRNOTAVAIL  } 
-- | address family not supported                   
#{enum UVErrno, UVErrno, uV_EAFNOSUPPORT   = UV_EAFNOSUPPORT   } 
-- | resource temporarily unavailable               
#{enum UVErrno, UVErrno, uV_EAGAIN         = UV_EAGAIN         } 
-- | address family not supported                   
#{enum UVErrno, UVErrno, uV_EAI_ADDRFAMILY = UV_EAI_ADDRFAMILY } 
-- | temporary failure                              
#{enum UVErrno, UVErrno, uV_EAI_AGAIN      = UV_EAI_AGAIN      } 
-- | bad ai_flags value                             
#{enum UVErrno, UVErrno, uV_EAI_BADFLAGS   = UV_EAI_BADFLAGS   } 
-- | invalid value for hints                        
#{enum UVErrno, UVErrno, uV_EAI_BADHINTS   = UV_EAI_BADHINTS   } 
-- | request canceled                               
#{enum UVErrno, UVErrno, uV_EAI_CANCELED   = UV_EAI_CANCELED   } 
-- | permanent failure                              
#{enum UVErrno, UVErrno, uV_EAI_FAIL       = UV_EAI_FAIL       } 
-- | ai_family not supported                        
#{enum UVErrno, UVErrno, uV_EAI_FAMILY     = UV_EAI_FAMILY     } 
-- | out of memory                                  
#{enum UVErrno, UVErrno, uV_EAI_MEMORY     = UV_EAI_MEMORY     } 
-- | no address                                     
#{enum UVErrno, UVErrno, uV_EAI_NODATA     = UV_EAI_NODATA     } 
-- | unknown node or service                        
#{enum UVErrno, UVErrno, uV_EAI_NONAME     = UV_EAI_NONAME     } 
-- | argument buffer overflow                       
#{enum UVErrno, UVErrno, uV_EAI_OVERFLOW   = UV_EAI_OVERFLOW   } 
-- | resolved protocol is unknown                   
#{enum UVErrno, UVErrno, uV_EAI_PROTOCOL   = UV_EAI_PROTOCOL   } 
-- | service not available for socket type          
#{enum UVErrno, UVErrno, uV_EAI_SERVICE    = UV_EAI_SERVICE    } 
-- | socket type not supported                      
#{enum UVErrno, UVErrno, uV_EAI_SOCKTYPE   = UV_EAI_SOCKTYPE   } 
-- | connection already in progress                 
#{enum UVErrno, UVErrno, uV_EALREADY       = UV_EALREADY       } 
-- | bad file descriptor                            
#{enum UVErrno, UVErrno, uV_EBADF          = UV_EBADF          } 
-- | resource busy or locked                        
#{enum UVErrno, UVErrno, uV_EBUSY          = UV_EBUSY          } 
-- | operation canceled                             
#{enum UVErrno, UVErrno, uV_ECANCELED      = UV_ECANCELED      } 
-- | invalid Unicode character                      
#{enum UVErrno, UVErrno, uV_ECHARSET       = UV_ECHARSET       } 
-- | software caused connection abort               
#{enum UVErrno, UVErrno, uV_ECONNABORTED   = UV_ECONNABORTED   } 
-- | connection refused                             
#{enum UVErrno, UVErrno, uV_ECONNREFUSED   = UV_ECONNREFUSED   } 
-- | connection reset by peer                       
#{enum UVErrno, UVErrno, uV_ECONNRESET     = UV_ECONNRESET     } 
-- | destination address required                   
#{enum UVErrno, UVErrno, uV_EDESTADDRREQ   = UV_EDESTADDRREQ   } 
-- | file already exists                            
#{enum UVErrno, UVErrno, uV_EEXIST         = UV_EEXIST         } 
-- | bad address in system call argument            
#{enum UVErrno, UVErrno, uV_EFAULT         = UV_EFAULT         } 
-- | file too large                                 
#{enum UVErrno, UVErrno, uV_EFBIG          = UV_EFBIG          } 
-- | host is unreachable                            
#{enum UVErrno, UVErrno, uV_EHOSTUNREACH   = UV_EHOSTUNREACH   } 
-- | interrupted system call                        
#{enum UVErrno, UVErrno, uV_EINTR          = UV_EINTR          } 
-- | invalid argument                               
#{enum UVErrno, UVErrno, uV_EINVAL         = UV_EINVAL         } 
-- | i/o error                                      
#{enum UVErrno, UVErrno, uV_EIO            = UV_EIO            } 
-- | socket is already connected                    
#{enum UVErrno, UVErrno, uV_EISCONN        = UV_EISCONN        } 
-- | illegal operation on a directory               
#{enum UVErrno, UVErrno, uV_EISDIR         = UV_EISDIR         } 
-- | too many symbolic links encountered            
#{enum UVErrno, UVErrno, uV_ELOOP          = UV_ELOOP          } 
-- | too many open files                            
#{enum UVErrno, UVErrno, uV_EMFILE         = UV_EMFILE         } 
-- | message too long                               
#{enum UVErrno, UVErrno, uV_EMSGSIZE       = UV_EMSGSIZE       } 
-- | name too long                                  
#{enum UVErrno, UVErrno, uV_ENAMETOOLONG   = UV_ENAMETOOLONG   } 
-- | network is down                                
#{enum UVErrno, UVErrno, uV_ENETDOWN       = UV_ENETDOWN       } 
-- | network is unreachable                         
#{enum UVErrno, UVErrno, uV_ENETUNREACH    = UV_ENETUNREACH    } 
-- | file table overflow                            
#{enum UVErrno, UVErrno, uV_ENFILE         = UV_ENFILE         } 
-- | no buffer space available                      
#{enum UVErrno, UVErrno, uV_ENOBUFS        = UV_ENOBUFS        } 
-- | no such device                                 
#{enum UVErrno, UVErrno, uV_ENODEV         = UV_ENODEV         } 
-- | no such file or directory                      
#{enum UVErrno, UVErrno, uV_ENOENT         = UV_ENOENT         } 
-- | not enough memory                              
#{enum UVErrno, UVErrno, uV_ENOMEM         = UV_ENOMEM         } 
-- | machine is not on the network                  
#{enum UVErrno, UVErrno, uV_ENONET         = UV_ENONET         } 
-- | protocol not available                         
#{enum UVErrno, UVErrno, uV_ENOPROTOOPT    = UV_ENOPROTOOPT    } 
-- | no space left on device                        
#{enum UVErrno, UVErrno, uV_ENOSPC         = UV_ENOSPC         } 
-- | function not implemented                       
#{enum UVErrno, UVErrno, uV_ENOSYS         = UV_ENOSYS         } 
-- | socket is not connected                        
#{enum UVErrno, UVErrno, uV_ENOTCONN       = UV_ENOTCONN       } 
-- | not a directory                                
#{enum UVErrno, UVErrno, uV_ENOTDIR        = UV_ENOTDIR        } 
-- | directory not empty                            
#{enum UVErrno, UVErrno, uV_ENOTEMPTY      = UV_ENOTEMPTY      } 
-- | socket operation on non-socket                 
#{enum UVErrno, UVErrno, uV_ENOTSOCK       = UV_ENOTSOCK       } 
-- | operation not supported on socket              
#{enum UVErrno, UVErrno, uV_ENOTSUP        = UV_ENOTSUP        } 
-- | operation not permitted                        
#{enum UVErrno, UVErrno, uV_EPERM          = UV_EPERM          } 
-- | broken pipe                                    
#{enum UVErrno, UVErrno, uV_EPIPE          = UV_EPIPE          } 
-- | protocol error                                 
#{enum UVErrno, UVErrno, uV_EPROTO         = UV_EPROTO         } 
-- | protocol not supported                         
#{enum UVErrno, UVErrno, uV_EPROTONOSUPPORT= UV_EPROTONOSUPPORT} 
-- | protocol wrong type for socket                 
#{enum UVErrno, UVErrno, uV_EPROTOTYPE     = UV_EPROTOTYPE     } 
-- | result too large                               
#{enum UVErrno, UVErrno, uV_ERANGE         = UV_ERANGE         } 
-- | read-only file system                          
#{enum UVErrno, UVErrno, uV_EROFS          = UV_EROFS          } 
-- | cannot send after transport endpoint shutdown  
#{enum UVErrno, UVErrno, uV_ESHUTDOWN      = UV_ESHUTDOWN      } 
-- | invalid seek                                   
#{enum UVErrno, UVErrno, uV_ESPIPE         = UV_ESPIPE         } 
-- | no such process                                
#{enum UVErrno, UVErrno, uV_ESRCH          = UV_ESRCH          } 
-- | connection timed out                           
#{enum UVErrno, UVErrno, uV_ETIMEDOUT      = UV_ETIMEDOUT      } 
-- | text file is busy                              
#{enum UVErrno, UVErrno, uV_ETXTBSY        = UV_ETXTBSY        } 
-- | cross-device link not permitted                
#{enum UVErrno, UVErrno, uV_EXDEV          = UV_EXDEV          } 
-- | unknown error                                  
#{enum UVErrno, UVErrno, uV_UNKNOWN        = UV_UNKNOWN        } 
-- | end of file                                    
#{enum UVErrno, UVErrno, uV_EOF            = UV_EOF            } 
-- | no such device or address                      
#{enum UVErrno, UVErrno, uV_ENXIO          = UV_ENXIO          } 
-- | too many links                                 
#{enum UVErrno, UVErrno, uV_EMLINK         = UV_EMLINK         }       


--------------------------------------------------------------------------------

-- | Wrapper libuv functions and throw appropriate exceptions.
-- 
-- Foreign functions from libuv which return `CInt` can indicate an error, 'throwUVErrorIfMinus' wrapper such 
-- functions and turn errnos into appropriate exceptions.
--
-- One exception is that we ignore EOF errors and return as it is, since generally EOF detection 
-- should be done in a different way.
--
throwUVErrorIfMinus :: CallStack -> String -> IO CInt -> IO CInt
throwUVErrorIfMinus cstack dev f = do
    errno <- f
    if errno >= 0
    then return errno
    else do
        desc <- (uv_strerror errno >>= peekCString)
        let uverrno = UVErrno errno
            info = IOEInfo uverrno desc dev cstack
        case () of
            _
                | uverrno == uV_EOF             -> return errno
                | uverrno == uV_E2BIG           -> throwIO (ResourceExhausted       info)
                | uverrno == uV_EACCES          -> throwIO (PermissionDenied        info)
                | uverrno == uV_EADDRINUSE      -> throwIO (ResourceBusy            info)
                | uverrno == uV_EADDRNOTAVAIL   -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAFNOSUPPORT    -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAGAIN          -> throwIO (ResourceExhausted       info)
                | uverrno == uV_EAI_ADDRFAMILY  -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAI_AGAIN       -> throwIO (ResourceExhausted       info)
                | uverrno == uV_EAI_BADFLAGS    -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAI_BADHINTS    -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAI_CANCELED    -> throwIO (ResourceVanished        info)
                | uverrno == uV_EAI_FAIL        -> throwIO (OtherError              info)
                | uverrno == uV_EAI_FAMILY      -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAI_MEMORY      -> throwIO (ResourceExhausted       info)
                | uverrno == uV_EAI_NODATA      -> throwIO (NoSuchThing             info)
                | uverrno == uV_EAI_NONAME      -> throwIO (NoSuchThing             info)
                | uverrno == uV_EAI_OVERFLOW    -> throwIO (InvalidArgument         info)
                | uverrno == uV_EAI_PROTOCOL    -> throwIO (ProtocolError           info)
                | uverrno == uV_EAI_SERVICE     -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EAI_SOCKTYPE    -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EALREADY        -> throwIO (AlreadyExists           info)
                | uverrno == uV_EBADF           -> throwIO (InvalidArgument         info)
                | uverrno == uV_EBUSY           -> throwIO (ResourceBusy            info)
                | uverrno == uV_ECANCELED       -> throwIO (ResourceVanished        info)
                | uverrno == uV_ECHARSET        -> throwIO (OtherError              info)
                | uverrno == uV_ECONNABORTED    -> throwIO (ResourceVanished        info)
                | uverrno == uV_ECONNREFUSED    -> throwIO (NoSuchThing             info)
                | uverrno == uV_ECONNRESET      -> throwIO (ResourceVanished        info)
                | uverrno == uV_EDESTADDRREQ    -> throwIO (InvalidArgument         info)
                | uverrno == uV_EEXIST          -> throwIO (AlreadyExists           info)
                | uverrno == uV_EFAULT          -> throwIO (OtherError              info)
                | uverrno == uV_EFBIG           -> throwIO (PermissionDenied        info)
                | uverrno == uV_EHOSTUNREACH    -> throwIO (NoSuchThing             info)
                | uverrno == uV_EINTR           -> throwIO (Interrupted             info)
                | uverrno == uV_EINVAL          -> throwIO (InvalidArgument         info)
                | uverrno == uV_EIO             -> throwIO (HardwareFault           info)
                | uverrno == uV_EISCONN         -> throwIO (AlreadyExists           info)
                | uverrno == uV_EISDIR          -> throwIO (InappropriateType       info)
                | uverrno == uV_ELOOP           -> throwIO (InvalidArgument         info)
                | uverrno == uV_EMFILE          -> throwIO (ResourceExhausted       info)
                | uverrno == uV_EMSGSIZE        -> throwIO (ResourceExhausted       info)
                | uverrno == uV_ENAMETOOLONG    -> throwIO (InvalidArgument         info)
                | uverrno == uV_ENETDOWN        -> throwIO (ResourceVanished        info)
                | uverrno == uV_ENETUNREACH     -> throwIO (NoSuchThing             info)
                | uverrno == uV_ENFILE          -> throwIO (ResourceExhausted       info)
                | uverrno == uV_ENOBUFS         -> throwIO (ResourceExhausted       info)
                | uverrno == uV_ENODEV          -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_ENOENT          -> throwIO (NoSuchThing             info)
                | uverrno == uV_ENOMEM          -> throwIO (ResourceExhausted       info)
                | uverrno == uV_ENOPROTOOPT     -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_ENOSPC          -> throwIO (ResourceExhausted       info)
                | uverrno == uV_ENOSYS          -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_ENOTCONN        -> throwIO (InvalidArgument         info)
                | uverrno == uV_ENOTDIR         -> throwIO (InappropriateType       info)
                | uverrno == uV_ENOTEMPTY       -> throwIO (UnsatisfiedConstraints  info)
                | uverrno == uV_ENOTSOCK        -> throwIO (InvalidArgument         info)
                | uverrno == uV_ENOTSUP         -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EPERM           -> throwIO (PermissionDenied        info)
                | uverrno == uV_EPIPE           -> throwIO (ResourceVanished        info)
                | uverrno == uV_EPROTO          -> throwIO (ProtocolError           info)
                | uverrno == uV_EPROTONOSUPPORT -> throwIO (ProtocolError           info)
                | uverrno == uV_EPROTOTYPE      -> throwIO (ProtocolError           info)
                | uverrno == uV_ERANGE          -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_EROFS           -> throwIO (PermissionDenied        info)
                | uverrno == uV_ESHUTDOWN       -> throwIO (IllegalOperation        info)
                | uverrno == uV_ESPIPE          -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_ESRCH           -> throwIO (NoSuchThing             info)
                | uverrno == uV_ETIMEDOUT       -> throwIO (TimeExpired             info)
                | uverrno == uV_ETXTBSY         -> throwIO (ResourceBusy            info)
                | uverrno == uV_EXDEV           -> throwIO (UnsupportedOperation    info)
                | uverrno == uV_UNKNOWN         -> throwIO (OtherError              info)
                | uverrno == uV_ENXIO           -> throwIO (NoSuchThing             info)
                | uverrno == uV_EMLINK          -> throwIO (ResourceExhausted       info)
                | otherwise                     -> throwIO (OtherError              info)
