{-# LANGUAGE ExistentialQuantification #-}

module System.IO.UV where

import Foreign
import Foreign.C

-- The macro `#alignment` exists since GHC 8.0
#if __GLASGOW_HASKELL < 800
# let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include <uv.h>

data UVLoop = forall a. UVLoop { uvLoopData :: Ptr a }

instance Storable UVLoop where
    sizeOf _ = #size uv_loop_t
    alignment _ = #alignment uv_loop_t
    poke p (UVLoop dataPtr) = do
        #{poke uv_loop_t, data} p $ dataPtr
    peek p = UVLoop 
        <$> (#{peek uv_loop_t, data} p)

#{enum CInt, CInt, 
  uV_RUN_DEFAULT = UV_RUN_DEFAULT,
  uV_RUN_ONCE    = UV_RUN_ONCE,
  uV_RUN_NOWAIT  = UV_RUN_NOWAIT}

foreign import ccall unsafe uv_loop_init      :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_loop_close     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_run            :: Ptr UVLoop -> CInt -> IO CInt
foreign import ccall unsafe uv_loop_alive     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_backend_fd     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_now            :: Ptr UVLoop -> IO CInt


data UVHandle = UVHandle
    { uvHandleData :: Int           -- the slot number
    , uvHandleType :: CInt
    , uvHandleLoop :: Ptr UVLoop
    }

instance Storable UVHandle where
    sizeOf _ = #size uv_handle_t
    alignment _ = #alignment uv_handle_t
    poke p uvhandle = do
        #{poke uv_handle_t, data} p $ uvHandleData uvhandle
        #{poke uv_handle_t, type} p $ uvHandleType uvhandle
        #{poke uv_handle_t, loop} p $ uvHandleLoop uvhandle
    peek p = UVHandle 
        <$> (#{peek uv_handle_t, data} p)
        <*> (#{peek uv_handle_t, type} p)
        <*> (#{peek uv_handle_t, loop} p)

#{enum CInt, CInt,
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
-- uv_stream_t

foreign import ccall unsafe hs_read_start :: Ptr stream -> IO ()


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


--------------------------------------------------------------------------------
-- uv_pipe_t

data UVPipe = UVPipe
    { uvPipeData :: CInt
    , uvPipeLoop :: Ptr UVLoop
    , uvPipeWriteQueueSize :: CSize
    }

instance Storable UVPipe where
    sizeOf _ = #size uv_pipe_t
    alignment _ = #alignment uv_pipe_t
    poke p uvpipe = do
        #{poke uv_pipe_t, data               } p $ uvPipeData uvpipe
        #{poke uv_pipe_t, loop               } p $ uvPipeLoop uvpipe
        #{poke uv_pipe_t, write_queue_size   } p $ uvPipeWriteQueueSize uvpipe
    peek p = UVPipe 
        <$> (#{peek uv_pipe_t, data               } p)
        <*> (#{peek uv_pipe_t, loop               } p)
        <*> (#{peek uv_pipe_t, write_queue_size   } p)

foreign import ccall unsafe uv_pipe_init  :: Ptr UVLoop -> Ptr UVPipe -> CInt -> IO ()
foreign import ccall unsafe uv_pipe_open  :: Ptr UVPipe -> UVFile -> IO ()
    
--------------------------------------------------------------------------------
-- uv_tty_t

data UVTTY = UVTTY
    { uvTTYData :: CInt
    , uvTTYLoop :: Ptr UVLoop
    , uvTTYWriteQueueSize :: CSize
    }

instance Storable UVTTY where
    sizeOf _ = #size uv_tty_t
    alignment _ = #alignment uv_tty_t
    poke p uvtty = do
        #{poke uv_tty_t, data               } p $ uvTTYData uvtty
        #{poke uv_tty_t, loop               } p $ uvTTYLoop uvtty
        #{poke uv_tty_t, write_queue_size   } p $ uvTTYWriteQueueSize uvtty
    peek p = UVTTY 
        <$> (#{peek uv_tty_t, data               } p)
        <*> (#{peek uv_tty_t, loop               } p)
        <*> (#{peek uv_tty_t, write_queue_size   } p)

poke_uv_tty_data :: Ptr UVTTY -> Int -> IO ()
poke_uv_tty_data p slot =  #{poke uv_tty_t, data} p slot 

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVTTY -> UVFile -> CInt -> IO ()

--------------------------------------------------------------------------------


type UVFile = CInt




