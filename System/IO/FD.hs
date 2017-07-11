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

Some functions require error message to provide better error reporting, the convention is that you should provide as
detailed error message as you could, it should contains caller and file specific informations,
something like "System.IO.File.close: /home/bob/test.txt" is OK.

Despite the /everything is a file/ philosophy, file descriptors operations are extremly hard to get right.
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
-- 'newFD' won't perform file lock for regular files, locking semantics should be implemented by wrapper types.
--
newFD :: CInt   -- ^ the file descriptor
      -> Bool   -- ^ is a socket (on Windows)
      -> Bool   -- ^ is in non-blocking mode on Unix
      -> FD
{-# INLINABLE newFD #-}
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
{-# INLINABLE fread #-}
fread loc !fd buf off len = readRawBufferPtr loc fd buf off (fromIntegral len)

-- | Write exaclty N bytes to 'FD'
--
fwrite :: String  -- ^ location message when error
        -> FD      -- ^ the file descriptor
        -> Ptr Word8  -- ^ the buffer
        -> Int        -- ^ buffer offset
        -> Int        -- ^ write length
        -> IO ()
{-# INLINABLE fwrite #-}
fwrite loc !fd buf off len = do
    res <- writeRawBufferPtr loc fd buf off (fromIntegral len)
    let res' = fromIntegral res
    when (res' < len) (fwrite loc fd (buf `plusPtr` res') 0 (len - res'))

-- | Close a 'FD'.
--
-- This operation is concurrency-safe, e.g. other threads blocked on this 'FD' will get an IO exception.
--
-- Note regular file using file locks should release lock before call 'fclose', otherwise if we're preempted
-- after closing but before releasing, the FD may have been reused, and we may release the wrong 'FD'.
--
fclose :: String -> FD -> IO ()
{-# INLINABLE fclose #-}
fclose loc fd = do
    let closer realFd =
            throwErrnoIfMinus1Retry_ loc $
#ifdef mingw32_HOST_OS
            if fdIsSocket fd then
                c_closesocket (fromIntegral realFd)
            else
#endif
                c_close (fromIntegral realFd)
    closeFdWith closer (fromIntegral (fdFD fd))

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
fseek :: String -> FD -> SeekMode -> Int -> IO ()
fseek loc fd mode off = throwErrnoIfMinus1Retry_ loc $
    c_lseek (fdFD fd) (fromIntegral off) seektype
  where
    seektype :: CInt
    seektype = case mode of
        AbsoluteSeek -> sEEK_SET
        RelativeSeek -> sEEK_CUR
        SeekFromEnd  -> sEEK_END

-- | Tell 'FD' current offset.
--
ftell :: String -> FD -> IO Int
ftell loc fd = fromIntegral `fmap` (throwErrnoIfMinus1Retry loc $
    c_lseek (fdFD fd) 0 sEEK_CUR)

-- | Get file size.
--
fgetSize :: String -> FD -> IO Int
fgetSize loc fd = do
    allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry_ loc $
            c_fstat (fdFD fd) p_stat
        c_mode <- st_mode p_stat :: IO CMode
        if not (s_isreg c_mode)
        then return (-1)
        else do
            c_size <- st_size p_stat
            return (fromIntegral c_size)

-- | Set file size.
--
fsetSize :: String -> FD -> Int -> IO ()
fsetSize loc fd size = throwErrnoIf_ (/=0) loc $
    c_ftruncate (fdFD fd) (fromIntegral size)

-- | Flush OS cache into disk.
--
fsync :: String -> FD -> IO ()
fsync loc fd = do
#ifdef mingw32_HOST_OS
    success <- c_FlushFileBuffers =<< c_get_osfhandle (fdFD fd)
    if success
    then return ()
    else do
        err_code <- c_GetLastError
        throwIO $ mkIOError (loc ++ ", error code is 0x" ++ showHex err_code)
                    Nothing (Just fp)
#else
    throwErrnoIfMinus1_ loc (c_fsync (fdFD fd))
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

--------------------------------------------------------------------------------

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

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
