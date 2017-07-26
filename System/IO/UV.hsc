{-# LANGUAGE ExistentialQuantification #-}

module System.IO.UV where

import Foreign
import Foreign.C

#include <uv.h>
#include <uv_hs.h>

--------------------------------------------------------------------------------

-- | This is data structure attach to uv_loop_t 's data field. It should be mirrored
-- to c struct in hs_uv.c.
--
data UVLoopData = UVLoopData
    { uVLoopEventCounter                       :: CSize           -- These two fields compose a special data structure
    , uvLoopEventQueue                         :: Ptr CSize       -- to keep trace of events during a uv_run
                                                                  -- before each uv_run the counter should be cleared
                                                                  --
    , uvLoopReadBufferTable                    :: Ptr (Ptr Word8) -- a list to keep read buffer's refrerence
    , uvLoopReadBufferSizeTable                :: Ptr CSize       -- a list to keep read buffer's size
    , uvLoopWriteBufferTable                   :: Ptr (Ptr Word8) -- a list to keep write buffer's refrerence
    , uvLoopWriteBufferSizeTable               :: Ptr CSize       -- a list to keep write buffer's size
    , uvLoopResultTable                        :: Ptr CSize       -- a list to keep callback's return value
                                                                  -- such as file or read bytes number
    } deriving Show

peekEventQueue :: Ptr UVLoopData -> IO (CSize, Ptr CSize)
peekEventQueue p = (,)
    <$> (#{peek hs_loop_data, event_counter          } p)
    <*> (#{peek hs_loop_data, event_queue            } p)

peekResultTable :: Ptr UVLoopData -> IO (Ptr CSize)
peekResultTable p = do
    (#{peek hs_loop_data, result_table          } p)


peekReadBuffer :: Ptr UVLoopData -> IO (Ptr (Ptr Word8), Ptr CSize)
peekReadBuffer p = (,)
    <$> (#{peek hs_loop_data, read_buffer_table          } p)
    <*> (#{peek hs_loop_data, read_buffer_size_table     } p)

clearUVLoopuEventCounter :: Ptr UVLoopData -> IO ()
clearUVLoopuEventCounter p = do
    #{poke hs_loop_data, event_counter          } p $ (0 :: CSize)


instance Storable UVLoopData where
    sizeOf _ = #size hs_loop_data
    alignment _ = #alignment hs_loop_data
    poke p d = do
        #{poke hs_loop_data, event_counter          } p $ uVLoopEventCounter          d
        #{poke hs_loop_data, event_queue            } p $ uvLoopEventQueue            d
        #{poke hs_loop_data, read_buffer_table      } p $ uvLoopReadBufferTable       d
        #{poke hs_loop_data, read_buffer_size_table } p $ uvLoopReadBufferSizeTable   d
        #{poke hs_loop_data, write_buffer_table     } p $ uvLoopWriteBufferTable      d
        #{poke hs_loop_data, write_buffer_size_table} p $ uvLoopWriteBufferSizeTable  d
        #{poke hs_loop_data, result_table           } p $ uvLoopResultTable           d
    peek p = UVLoopData 
        <$> (#{peek hs_loop_data, event_counter          } p)
        <*> (#{peek hs_loop_data, event_queue            } p)
        <*> (#{peek hs_loop_data, read_buffer_table      } p)
        <*> (#{peek hs_loop_data, read_buffer_size_table } p)
        <*> (#{peek hs_loop_data, write_buffer_table     } p)
        <*> (#{peek hs_loop_data, write_buffer_size_table} p)
        <*> (#{peek hs_loop_data, result_table           } p)

--------------------------------------------------------------------------------

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

foreign import ccall unsafe uv_run            :: Ptr UVLoop -> CInt -> IO CInt
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> CInt -> IO CInt


foreign import ccall unsafe uv_loop_init      :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_loop_close     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_loop_alive     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_backend_fd     :: Ptr UVLoop -> IO CInt
foreign import ccall unsafe uv_now            :: Ptr UVLoop -> IO CULong


data UVHandle = UVHandle
    { uvHandleData :: Int           -- the slot number
    , uvHandleType :: CInt
    , uvHandleLoop :: Ptr UVLoop
    }

poke_uv_handle_data :: Ptr UVHandle -> CSize -> IO ()
poke_uv_handle_data p slot =  #{poke uv_handle_t, data} p slot 

foreign import ccall uv_ref :: Ptr UVHandle -> IO ()
foreign import ccall uv_unref :: Ptr UVHandle -> IO ()
foreign import ccall uv_close :: Ptr UVHandle -> FunPtr a -> IO ()

foreign import ccall uv_handle_size :: CInt -> IO CSize

mallocUVHandle :: UVHandleType -> IO (ForeignPtr UVHandle)
mallocUVHandle (UVHandleType typ) = 
    mallocForeignPtrBytes . fromIntegral =<< uv_handle_size typ

newtype UVHandleType = UVHandleType { getUVHandleType :: CInt } deriving (Show, Eq, Ord)

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
-- uv_timer_t

foreign import ccall unsafe uv_timer_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_timer_start_no_callback :: Ptr UVHandle -> CULong -> IO CInt
foreign import ccall unsafe uv_timer_stop :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_timer_start_stop_loop :: Ptr UVHandle -> CULong -> IO CInt

--------------------------------------------------------------------------------
-- uv_async_t

foreign import ccall unsafe hs_async_init_no_callback :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_async_send :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------
-- uv_signal_t

foreign import ccall unsafe uv_signal_init :: Ptr UVLoop -> Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_signal_start_no_callback :: Ptr UVHandle -> CInt -> IO ()

--------------------------------------------------------------------------------
-- uv_stream_t

foreign import ccall unsafe hs_read_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_stream_set_blocking :: Ptr UVHandle -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- uv_tcp_t

foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tcp_open :: Ptr UVHandle -> CInt -> IO CInt


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

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> UVFile -> CInt -> IO CInt

--------------------------------------------------------------------------------


type UVFile = CInt




