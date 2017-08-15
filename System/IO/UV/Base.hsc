{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.IO.UV.Base where

import Foreign
import Foreign.C
import System.IO.UV.Exception (UVReturn(..))
import System.Posix.Types (CSsize(..))

-- The macro `#alignment` exists since GHC 8.0
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include <uv.h>
#include <hs_uv.h>

newtype UVSlot = UVSlot CSize 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

--------------------------------------------------------------------------------

data UVLoopData

peekUVEventQueue :: Ptr UVLoopData -> IO (CSize, Ptr CSize)
peekUVEventQueue p = (,)
    <$> (#{peek hs_loop_data, event_counter          } p)
    <*> (#{peek hs_loop_data, event_queue            } p)

clearUVEventCounter :: Ptr UVLoopData -> IO ()
clearUVEventCounter p = do
    #{poke hs_loop_data, event_counter          } p $ (0 :: CSize)

peekUVResultTable :: Ptr UVLoopData -> IO (Ptr CSsize)
peekUVResultTable p = do
    (#{peek hs_loop_data, result_table          } p)

peekUVBufferTable :: Ptr UVLoopData -> IO (Ptr (Ptr Word8), Ptr CSize)
peekUVBufferTable p = (,)
    <$> (#{peek hs_loop_data, buffer_table          } p)
    <*> (#{peek hs_loop_data, buffer_size_table     } p)

--------------------------------------------------------------------------------

data UVLoop

newtype UVRunMode = UVRunMode CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVRunMode, UVRunMode, 
  uV_RUN_DEFAULT = UV_RUN_DEFAULT,
  uV_RUN_ONCE    = UV_RUN_ONCE,
  uV_RUN_NOWAIT  = UV_RUN_NOWAIT}

peek_uv_loop_data :: Ptr UVLoop -> IO (Ptr UVLoopData)
peek_uv_loop_data p = #{peek uv_loop_t, data} p

foreign import ccall unsafe hs_loop_init      :: CSize -> IO (Ptr UVLoop)
foreign import ccall safe hs_loop_close       :: Ptr UVLoop -> IO ()
foreign import ccall unsafe hs_loop_resize    :: Ptr UVLoop -> CSize -> IO (Ptr UVLoop)

foreign import ccall unsafe uv_run            :: Ptr UVLoop -> UVRunMode -> IO (UVReturn CInt)
-- we never use safe blocking wait actually
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO (UVReturn CInt)

foreign import ccall unsafe uv_loop_alive     :: Ptr UVLoop -> IO (UVReturn CInt)
foreign import ccall unsafe uv_backend_fd     :: Ptr UVLoop -> IO (UVReturn CInt)
foreign import ccall unsafe uv_now            :: Ptr UVLoop -> IO (UVReturn CULong)

--------------------------------------------------------------------------------

data UVHandle

poke_uv_handle_data :: Ptr UVHandle -> UVSlot -> IO ()
poke_uv_handle_data p slot =  #{poke uv_handle_t, data} p slot 

foreign import ccall unsafe hs_handle_init  :: UVHandleType -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_handle_close :: Ptr UVHandle -> IO ()

foreign import ccall unsafe uv_ref :: Ptr UVHandle -> IO ()
foreign import ccall unsafe uv_unref :: Ptr UVHandle -> IO ()

newtype UVHandleType = UVHandleType CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVHandleType, UVHandleType,
    uV_UNKNOWN_HANDLE  = UV_UNKNOWN_HANDLE,
    uV_ASYNC           = UV_ASYNC,
    uV_CHECK           = UV_CHECK,
    uV_FS_EVENT        = UV_FS_EVENT,
    uV_FS_POLL         = UV_FS_POLL,
    uV_HANDLE          = UV_HANDLE,
    uV_IDLE            = UV_IDLE,
    uV_NAMED_PIPE      = UV_NAMED_PIPE,
    uV_POLL            = UV_POLL,
    uV_PREPARE         = UV_PREPARE,
    uV_PROCESS         = UV_PROCESS,
    uV_STREAM          = UV_STREAM,
    uV_TCP             = UV_TCP,
    uV_TIMER           = UV_TIMER,
    uV_TTY             = UV_TTY,
    uV_UDP             = UV_UDP,
    uV_SIGNAL          = UV_SIGNAL,
    uV_FILE            = UV_FILE,
    uV_HANDLE_TYPE_MAX = UV_HANDLE_TYPE_MAX }

--------------------------------------------------------------------------------
-- uv_req_t

data UVReq

poke_uv_req_data :: Ptr UVReq -> CSize -> IO ()
poke_uv_req_data p slot =  #{poke uv_req_t, data} p slot 

foreign import ccall unsafe hs_req_init :: UVReqType -> IO (Ptr UVReq)
foreign import ccall unsafe hs_req_free :: Ptr UVReq -> IO ()

newtype UVReqType = UVReqType CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVReqType, UVReqType,
    uV_UNKNOWN_REQ      = UV_UNKNOWN_REQ,
    uV_REQ              = UV_REQ,
    uV_CONNECT          = UV_CONNECT,
    uV_WRITE            = UV_WRITE,
    uV_SHUTDOWN         = UV_SHUTDOWN,
    uV_UDP_SEND         = UV_UDP_SEND,
    uV_FS               = UV_FS,
    uV_WORK             = UV_WORK,
    uV_GETADDRINFO      = UV_GETADDRINFO,
    uV_GETNAMEINFO      = UV_GETNAMEINFO,
    uV_REQ_TYPE_MAX     = UV_REQ_TYPE_MAX }

--------------------------------------------------------------------------------
-- uv_stream_t

foreign import ccall unsafe uv_stream_set_blocking :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
foreign import ccall unsafe hs_read_start :: Ptr UVHandle -> IO (UVReturn CInt)
foreign import ccall unsafe hs_listen :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
foreign import ccall unsafe hs_write :: Ptr UVReq -> Ptr UVHandle -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_tcp_t

foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO (UVReturn CInt)
foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO (UVReturn CInt)

-- see notes in cbits/hs_uv.c
#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "uv_tcp_open_win32" uv_tcp_open :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
#else
foreign import ccall unsafe uv_tcp_open :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
#endif

foreign import ccall unsafe uv_tcp_simultaneous_accepts :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
foreign import ccall unsafe uv_tcp_nodelay :: Ptr UVHandle -> CInt -> IO (UVReturn CInt)
foreign import ccall unsafe uv_tcp_keepalive :: Ptr UVHandle -> CInt -> CUInt -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_poll_t

newtype UVPollEvent = UVPollEvent CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVPollEvent, UVPollEvent,
   uV_READABLE    = UV_READABLE,
   uV_WRITABLE    = UV_WRITABLE}

foreign import ccall unsafe uv_poll_init_socket :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO (UVReturn CInt)
foreign import ccall unsafe hs_poll_start :: Ptr UVHandle -> UVPollEvent -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_pipe_t

foreign import ccall unsafe uv_pipe_init  :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO ()
foreign import ccall unsafe uv_pipe_open  :: Ptr UVHandle -> UVFile -> IO ()
    
--------------------------------------------------------------------------------
-- uv_tty_t

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> UVFile -> CInt -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_timer_t

foreign import ccall unsafe uv_timer_init :: Ptr UVLoop -> Ptr UVHandle -> IO (UVReturn CInt)
foreign import ccall unsafe uv_timer_stop :: Ptr UVHandle -> IO (UVReturn CInt)
foreign import ccall unsafe hs_timer_start_stop_loop :: Ptr UVHandle -> CULong -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_async_t

foreign import ccall unsafe hs_async_init_stop_loop :: Ptr UVLoop -> Ptr UVHandle -> IO (UVReturn CInt)
foreign import ccall unsafe uv_async_send :: Ptr UVHandle -> IO (UVReturn CInt)

--------------------------------------------------------------------------------
-- uv_signal_t

foreign import ccall unsafe uv_signal_init :: Ptr UVLoop -> Ptr UVHandle -> IO ()

--------------------------------------------------------------------------------

data UVTimespec = UVTimespec
    { uvtSec :: CLong
    , uvtNsec :: CLong
    }

instance Storable UVTimespec where
    sizeOf _ = #size uv_timespec_t
    alignment _ = #alignment uv_timespec_t
    poke p uvt = do
        #{poke uv_timespec_t, tv_sec} p $ uvtSec uvt
        #{poke uv_timespec_t, tv_nsec} p $ uvtNsec uvt
    peek p = UVTimespec 
        <$> (#{peek uv_timespec_t, tv_sec} p)
        <*> (#{peek uv_timespec_t, tv_nsec} p)


data UVStat = UVStat
    { uvstDev      :: CLLong
    , uvstMode     :: CLLong
    , uvstNlink    :: CLLong
    , uvstUid      :: CLLong
    , uvstGid      :: CLLong
    , uvstRdev     :: CLLong
    , uvstIno      :: CLLong
    , uvstSize     :: CLLong
    , uvstBlksize  :: CLLong
    , uvstBlocks   :: CLLong
    , uvstFlags    :: CLLong
    , uvstGen      :: CLLong
    , uvstAtim     :: UVTimespec
    , uvstMtim     :: UVTimespec
    , uvstCtim     :: UVTimespec
    , uvstBirthtim :: UVTimespec
    }

#{enum CInt, CInt,
   uV_FS_UNKNOWN  = UV_FS_UNKNOWN,
   uV_FS_CUSTOM   = UV_FS_CUSTOM,
   uV_FS_OPEN     = UV_FS_OPEN,
   uV_FS_CLOSE    = UV_FS_CLOSE,
   uV_FS_READ     = UV_FS_READ,
   uV_FS_WRITE    = UV_FS_WRITE,
   uV_FS_SENDFILE = UV_FS_SENDFILE,
   uV_FS_STAT     = UV_FS_STAT,
   uV_FS_LSTAT    = UV_FS_LSTAT,
   uV_FS_FSTAT    = UV_FS_FSTAT,
   uV_FS_FTRUNCATE= UV_FS_FTRUNCATE,
   uV_FS_UTIME    = UV_FS_UTIME,
   uV_FS_FUTIME   = UV_FS_FUTIME,
   uV_FS_ACCESS   = UV_FS_ACCESS,
   uV_FS_CHMOD    = UV_FS_CHMOD,
   uV_FS_FCHMOD   = UV_FS_FCHMOD,
   uV_FS_FSYNC    = UV_FS_FSYNC,
   uV_FS_FDATASYNC= UV_FS_FDATASYNC,
   uV_FS_UNLINK   = UV_FS_UNLINK,
   uV_FS_RMDIR    = UV_FS_RMDIR,
   uV_FS_MKDIR    = UV_FS_MKDIR,
   uV_FS_MKDTEMP  = UV_FS_MKDTEMP,
   uV_FS_RENAME   = UV_FS_RENAME,
   uV_FS_SCANDIR  = UV_FS_SCANDIR,
   uV_FS_LINK     = UV_FS_LINK,
   uV_FS_SYMLINK  = UV_FS_SYMLINK,
   uV_FS_READLINK = UV_FS_READLINK,
   uV_FS_CHOWN    = UV_FS_CHOWN,
   uV_FS_FCHOWN   = UV_FS_FCHOWN,
   uV_FS_REALPATH = UV_FS_REALPATH }

#{enum CInt, CInt,
   uV_DIRENT_UNKNOWN = UV_DIRENT_UNKNOWN,
   uV_DIRENT_FILE    = UV_DIRENT_FILE,
   uV_DIRENT_DIR     = UV_DIRENT_DIR,
   uV_DIRENT_LINK    = UV_DIRENT_LINK,
   uV_DIRENT_FIFO    = UV_DIRENT_FIFO,
   uV_DIRENT_SOCKET  = UV_DIRENT_SOCKET,
   uV_DIRENT_CHAR    = UV_DIRENT_CHAR,
   uV_DIRENT_BLOCK   = UV_DIRENT_BLOCK }

instance Storable UVStat where
    sizeOf _ = #size uv_stat_t
    alignment _ = #alignment uv_stat_t
    poke p uvt = do
        #{poke uv_stat_t, st_dev      } p $ uvstDev       uvt
        #{poke uv_stat_t, st_mode     } p $ uvstMode      uvt
        #{poke uv_stat_t, st_nlink    } p $ uvstNlink     uvt
        #{poke uv_stat_t, st_uid      } p $ uvstUid       uvt
        #{poke uv_stat_t, st_gid      } p $ uvstGid       uvt
        #{poke uv_stat_t, st_rdev     } p $ uvstRdev      uvt
        #{poke uv_stat_t, st_ino      } p $ uvstIno       uvt
        #{poke uv_stat_t, st_size     } p $ uvstSize      uvt
        #{poke uv_stat_t, st_blksize  } p $ uvstBlksize   uvt
        #{poke uv_stat_t, st_blocks   } p $ uvstBlocks    uvt
        #{poke uv_stat_t, st_flags    } p $ uvstFlags     uvt
        #{poke uv_stat_t, st_gen      } p $ uvstGen       uvt
        #{poke uv_stat_t, st_atim     } p $ uvstAtim      uvt
        #{poke uv_stat_t, st_mtim     } p $ uvstMtim      uvt
        #{poke uv_stat_t, st_ctim     } p $ uvstCtim      uvt
        #{poke uv_stat_t, st_birthtim } p $ uvstBirthtim  uvt
    peek p = UVStat 
        <$> (#{peek uv_stat_t, st_dev      } p)
        <*> (#{peek uv_stat_t, st_mode     } p)
        <*> (#{peek uv_stat_t, st_nlink    } p)
        <*> (#{peek uv_stat_t, st_uid      } p)
        <*> (#{peek uv_stat_t, st_gid      } p)
        <*> (#{peek uv_stat_t, st_rdev     } p)
        <*> (#{peek uv_stat_t, st_ino      } p)
        <*> (#{peek uv_stat_t, st_size     } p)
        <*> (#{peek uv_stat_t, st_blksize  } p)
        <*> (#{peek uv_stat_t, st_blocks   } p)
        <*> (#{peek uv_stat_t, st_flags    } p)
        <*> (#{peek uv_stat_t, st_gen      } p)
        <*> (#{peek uv_stat_t, st_atim     } p)
        <*> (#{peek uv_stat_t, st_mtim     } p)
        <*> (#{peek uv_stat_t, st_ctim     } p)
        <*> (#{peek uv_stat_t, st_birthtim } p)

data UVFs = UVFs
    { uvFsData :: CInt
    , uvFsLoop :: Ptr UVLoop
    , uvFsType :: CInt
    , uvFsPath :: CString   -- TODO: windows need to use CWString
    , uvFsResult :: CSize
    , uvFsStat :: UVStat  
    , uvFsPtr  :: Ptr ()
    }

instance Storable UVFs where
    sizeOf _ = #size uv_fs_t
    alignment _ = #alignment uv_fs_t
    poke p uvfs = do
        #{poke uv_fs_t, data   } p $ uvFsData uvfs
        #{poke uv_fs_t, loop   } p $ uvFsLoop uvfs
        #{poke uv_fs_t, fs_type} p $ uvFsType uvfs
        #{poke uv_fs_t, path   } p $ uvFsPath uvfs
        #{poke uv_fs_t, result } p $ uvFsResult uvfs
        #{poke uv_fs_t, statbuf} p $ uvFsStat uvfs
        #{poke uv_fs_t, ptr    } p $ uvFsPtr uvfs
    peek p = UVFs 
        <$> (#{peek uv_fs_t, data   } p)
        <*> (#{peek uv_fs_t, loop   } p)
        <*> (#{peek uv_fs_t, fs_type} p)
        <*> (#{peek uv_fs_t, path   } p)
        <*> (#{peek uv_fs_t, result } p)
        <*> (#{peek uv_fs_t, statbuf} p)
        <*> (#{peek uv_fs_t, ptr    } p)

foreign import ccall unsafe uv_fs_req_cleanup :: Ptr UVFs -> IO ()
foreign import ccall unsafe uv_fs_close       :: Ptr UVLoop -> Ptr UVFs -> UVFile -> IO ()
foreign import ccall unsafe uv_fs_open_hs     :: Ptr UVLoop -> Ptr UVFs -> CString -> CInt -> CInt -> IO ()




type UVFile = CInt



