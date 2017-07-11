{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : System.IO.FD
Description : Basic file descriptor operations
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide low-level operations on file descriptors to help you define your own I/O devices,
these operations are not protected by any locks, you might want to protect them with MVar to provide certain
concurrent semantics. Users are encouraged to use wrappered device types instead of dealing with file descriptors directly.

Despite of the /everything is a file/ philosophy, file descriptors operations are extremly hard to get right.
This module re-exports most functions from "GHC.IO.FD" module, which will:

  * Correctly handle non-threaded and threaded runtime so that other haskell threads won't be blocked.
  * Correctly handle windows and unix differences, e.g. sockets need extra care on windows.
  * Correctly handle both O_NONBLOCK and O_BLOCK file descriptors, which is important for unix standard streams.
  * Use OS I/O multiplexer where possible, currently use epoll on linux, and kqueue on other unix environment.

For file descriptors which I/O multiplexer might not work for, for example regular files, cares should be taken: don't
fork too many haskell threads do concurrent operations, since doing that will either block too many capacities, or spawn too
many OS threads. For regular files case, a proper level of concurrency might even improve I/O performance!

Please take a look at unix man page for details on each operations, which are many!

-}

module System.IO.FD (
  -- * FD type and universal operations
    FD
  , fdFD
  , newFD
  , fread
  , fwrite
  , fclose
  , fdup
  , fdup2
  -- * Regular file operations
  , SeekMode(..)
  , fseekable
  , fseek
  , ftell
  , fgetSize
  , fsetSize
  , fsync
  -- * TTY operations
  , fterminal
  , fgetEcho
  , fsetEcho
  , fsetCanonical
  ) where


import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent (rtsSupportsBoundThreads)
import System.Posix.Types
import System.Posix.Internals hiding (FD, setEcho, getEcho)
import Foreign.C
import Foreign.Marshal
import Foreign.C.Types
import Foreign.Ptr
import GHC.Conc.IO
import GHC.IO.FD
import GHC.IO.Device
import Data.Word

-- | Make a new 'FD' from OS file descriptor.
--
newFD :: CInt   -- ^ the file descriptor
      -> Bool   -- ^ is a socket (on Windows)
      -> Bool   -- ^ is in non-blocking mode on Unix
      -> FD
newFD fd is_socket is_nonblock =
    FD{ fdFD = fd,
#ifndef mingw32_HOST_OS
        fdIsNonBlocking = fromEnum is_nonblock
#else
        fdIsSocket_ = fromEnum is_socket
#endif
      }


-- | Read up to N bytes from a 'FD'.
--
--
fread :: String  -- ^ location message when error
      -> FD      -- ^ the file descriptor
      -> Ptr Word8  -- ^ the buffer
      -> Int        -- ^ buffer offset
      -> Int        -- ^ read limit
      -> IO Int     -- ^ actual bytes read
fread loc !fd buf off len = readRawBufferPtr loc fd buf off (fromIntegral len)

-- | Write exaclty N bytes to 'FD'
--
fwrite :: String  -- ^ location message when error
        -> FD      -- ^ the file descriptor
        -> Ptr Word8  -- ^ the buffer
        -> Int        -- ^ buffer offset
        -> Int        -- ^ write length
        -> IO ()
fwrite loc !fd buf off len = do
    res <- writeRawBufferPtr loc fd buf off (fromIntegral len)
    let res' = fromIntegral res
    when (res' < len) (fwrite loc fd (buf `plusPtr` res') 0 (len - res'))

-- | Close a 'FD'.
--
-- This operation is concurrency-safe, e.g. other threads blocked on this 'FD' will get an IO exception.
--
fclose :: FD -> IO ()
fclose = close

-- | Duplicate 'FD'.
--
fdup :: FD -> IO FD
fdup = dup

-- | Duplicate the first 'FD' to the second 'FD', the second 'FD' is sliently closed if open.
--
fdup2 :: FD -> FD -> IO FD
fdup2 = dup2

-- | Is 'FD' seekable?
--
fseekable :: FD -> IO Bool
fseekable = isSeekable

-- | Seeking 'FD'.
fseek :: FD -> SeekMode -> Int -> IO ()
fseek fd mode off = throwErrnoIfMinus1Retry_ "seek" $
    c_lseek (fdFD fd) (fromIntegral off) seektype
  where
    seektype :: CInt
    seektype = case mode of
        AbsoluteSeek -> sEEK_SET
        RelativeSeek -> sEEK_CUR
        SeekFromEnd  -> sEEK_END

-- | Tell 'FD' current offset.
--
ftell :: FD -> IO Int
ftell fd = fromIntegral `fmap` (throwErrnoIfMinus1Retry "hGetPosn" $
    c_lseek (fdFD fd) 0 sEEK_CUR)

-- | Get file size.
--
fgetSize :: FD -> IO Int
fgetSize fd = do
    allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry_ "fileSize" $
            c_fstat (fdFD fd) p_stat
        c_mode <- st_mode p_stat :: IO CMode
        if not (s_isreg c_mode)
        then return (-1)
        else do
            c_size <- st_size p_stat
            return (fromIntegral c_size)

-- | Set file size.
--
fsetSize :: FD -> Int -> IO ()
fsetSize fd size = throwErrnoIf_ (/=0) "GHC.IO.FD.setSize" $
    c_ftruncate (fdFD fd) (fromIntegral size)

-- | Flush OS cache into disk.
--
fsync :: FD -> IO ()
fsync fd = do
#ifdef mingw32_HOST_OS
    success <- c_FlushFileBuffers =<< c_get_osfhandle (fdFD fd)
    if success
    then return ()
    else do
        err_code <- c_GetLastError
        throwIO $ mkIOError ("System.IO.File.fsync: error code is 0x" ++ showHex err_code)
                    Nothing (Just fp)
#else
    throwErrnoIfMinus1_ "fileSynchronise" (c_fsync (fdFD fd))
#endif

-- | Get device type.
--
-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
--
fdeviceType :: FD -> IO IODeviceType
fdeviceType = devType

-- | Is this 'FD' a pointer?
--
fterminal :: FD -> IO Bool
fterminal = isTerminal

-- | Get terminal echo state.
--
fgetEcho :: FD -> IO Bool
fgetEcho = getEcho

-- | Set terminal echo state.
--
fsetEcho :: FD -> Bool -> IO ()
fsetEcho = setEcho

-- | Set terminal to canonical / non-canonical mode.
--
fsetCanonical :: FD -> Bool -> IO ()
fsetCanonical = setRaw

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_get_osfhandle"
    c_get_osfhandle :: CInt -> IO (Ptr ())

foreign import WINDOWS_CCONV unsafe "windows.h FlushFileBuffers"
	c_FlushFileBuffers :: Ptr () -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
    c_GetLastError :: IO Word32
#else
foreign import capi safe "unistd.h fsync"
    c_fsync :: CInt  -> IO CInt
#endif

