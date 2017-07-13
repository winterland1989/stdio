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
these operations may only works for certain type of file descriptors due to OS limitations, and are not protected by any locks,
you have to protect them with MVar to provide certain concurrent semantics.
Users are encouraged to use wrappered device types instead of dealing with file descriptors directly.

Operations use 'HasCallStack' to provide better error reporting, the convention is that you also provide detail device informations,
such as file name or socket address.

Despite the /everything is a file/ philosophy, file descriptors operations are extremly hard to get right.
Operations in this module will:

  * Correctly handle non-threaded and threaded runtime so that other haskell threads won't be blocked.
  * Use OS I/O multiplexer where possible, currently use epoll on linux, and kqueue on other unix environment.

For file descriptors which I/O multiplexer might not work for, for example regular files, cares should be taken: don't
fork too many haskell threads do concurrent operations, since doing that will either block too many capacities, or spawn too
many OS threads. For regular files case, a proper level of concurrency might even improve I/O performance!

Please take a look at unix man page for details on each operations, which are many!

-}

module System.IO.FD (
  -- * FD type and universal operations
    FD
  , fread
  , freadBlock
  , fwrite
  , fwriteBlock
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
import Control.Exception
import System.IO.Exception
import Control.Concurrent.MVar
import Control.Concurrent (rtsSupportsBoundThreads)
import System.Posix.Types
import System.Posix.Internals
import Foreign.Marshal
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Conc.IO
import GHC.IO.Device (IODeviceType(..), SeekMode(..))
import GHC.Stack
import Data.Word
import Data.Bits
import Numeric (showHex)


-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdStat :: HasCallStack => String -> FD -> IO (IODeviceType, CDev, CIno)
fdStat dev fd =
    allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry_ callStack dev $
            c_fstat fd p_stat
        ty <- statGetType p_stat
        dev <- st_dev p_stat
        ino <- st_ino p_stat
        return (ty,dev,ino)


-- | Get device type.
--
-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
--
fdeviceType :: FD -> IO IODeviceType
fdeviceType fd = do (ty,_,_) <- fdStat fd; return ty


-- | Read up to N bytes from a 'FD'.
--
-- The 'FD' should be opened with 'O_NONBLOCK' if it can block, otherwise 'fread' would block RTS.
-- All 'FD' we create in this package are marked with 'O_NONBLOCK'(except standard streams).
--
fread :: HasCallStack
      => String  -- ^ device info
      -> FD      -- ^ the file descriptor
      -> Ptr Word8  -- ^ the buffer
      -> Int        -- ^ buffer offset
      -> Int        -- ^ read limit
      -> IO Int     -- ^ actual bytes read
{-# INLINABLE fread #-}
fread dev !fd buf off len = do
#ifndef mingw32_HOST_OS
    unsafe_read
  where
    do_read call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock callStack dev call
                            (threadWaitRead (fromIntegral fd))
    unsafe_read = do_read (c_read fd (buf `plusPtr` off) (fromIntegral len))
#else /* mingw32_HOST_OS.... */
    if rtsSupportsBoundThreads
    then blockingReadRawBufferPtr callStack dev fd buf off len
    else asyncReadRawBufferPtr    callStack dev fd buf off len
#endif

-- | Read up to N bytes from a 'FD'.
--
-- The 'FD' could be opened with 'O_BLOCK', we do a select before reading. This functions is mainly used for
-- standard streams since we don't want to change their flags.
--
freadBlock :: HasCallStack
           => String     -- ^ device info
           -> FD         -- ^ the file descriptor
           -> Ptr Word8  -- ^ the buffer
           -> Int        -- ^ buffer offset
           -> Int        -- ^ read limit
           -> IO Int     -- ^ actual bytes read
{-# INLINABLE freadBlock #-}
freadBlock dev !fd buf off len = do
#ifndef mingw32_HOST_OS
    r <- throwErrnoIfMinus1 callStack dev (unsafe_fdReady fd 0 0 0)
    if r /= 0
    then read
    else do threadWaitRead (fromIntegral fd); read
  where
    do_read call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock callStack dev call
                            (threadWaitRead (fromIntegral fd))
    read        = if rtsSupportsBoundThreads then safe_read else unsafe_read
    unsafe_read = do_read (c_read fd (buf `plusPtr` off) (fromIntegral len))
    safe_read   = do_read (c_safe_read fd (buf `plusPtr` off) (fromIntegral len))
#else /* mingw32_HOST_OS.... */
    if rtsSupportsBoundThreads
    then blockingReadRawBufferPtr callStack dev fd buf off len
    else asyncReadRawBufferPtr    callStack dev fd buf off len
#endif

-- | Write exaclty N bytes to 'FD'
--
fwrite :: HasCallStack
       => String  -- ^ device info
       -> FD      -- ^ the file descriptor
       -> Ptr Word8  -- ^ the buffer
       -> Int        -- ^ buffer offset
       -> Int        -- ^ write length
       -> IO Int
{-# INLINABLE fwrite #-}
fwrite dev !fd buf off len = do
#ifndef mingw32_HOST_OS
    unsafe_write -- unsafe is ok, it can't block
  where
    do_write call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock callStack dev call
                        (threadWaitWrite (fromIntegral fd))
    unsafe_write  = do_write (c_write fd (buf `plusPtr` off) (fromIntegral len))
#else /* mingw32_HOST_OS.... */
    if rtsSupportsBoundThreads
    then blockingWriteRawBufferPtr callStack dev fd buf off len
    else asyncWriteRawBufferPtr    callStack dev fd buf off len
#endif

-- | Write exaclty N bytes to 'FD'
--
fwriteBlock :: HasCallStack
            => String  -- ^ location message when error
            -> FD      -- ^ the file descriptor
            -> Ptr Word8  -- ^ the buffer
            -> Int        -- ^ buffer offset
            -> Int        -- ^ write length
            -> IO Int
{-# INLINABLE fwriteBlock #-}
fwriteBlock dev !fd buf off len = do
#ifndef mingw32_HOST_OS
    r <- unsafe_fdReady fd 1 0 0
    if r /= 0
    then write
    else do threadWaitWrite (fromIntegral fd); write
  where
    do_write call = fromIntegral `fmap`
                        throwErrnoIfMinus1RetryMayBlock callStack dev call
                            (threadWaitWrite (fromIntegral fd))
    write         = if rtsSupportsBoundThreads then safe_write else unsafe_write
    unsafe_write  = do_write (c_write fd (buf `plusPtr` off) (fromIntegral len))
    safe_write    = do_write (c_safe_write fd (buf `plusPtr` off) (fromIntegral len))
#else /* mingw32_HOST_OS.... */
    if rtsSupportsBoundThreads
    then blockingWriteRawBufferPtr callStack dev fd buf off len
    else asyncWriteRawBufferPtr    callStack dev fd buf off len
#endif


-- | Close a 'FD'.
--
-- This operation is concurrency-safe, e.g. other threads blocked on this 'FD' will get an IO exception.
--
-- Note if 'FD' is socket then you should use 'closeSocket' instead of this function.
--
fclose :: HasCallStack => String -> FD -> IO ()
{-# INLINABLE fclose #-}
fclose dev fd = do
    let closer realFD =
            throwErrnoIfMinus1Retry_ callStack dev $
                c_close (fromIntegral realFD)
    closeFdWith closer (fromIntegral fd)

-- | Duplicate 'FD'.
--
fdup :: HasCallStack => FD -> IO FD
fdup fd = do
    newfd <- throwErrnoIfMinus1 callStack "dup device" $ c_dup fd
    return newfd

-- | Duplicate the first 'FD' to the second 'FD', the second 'FD' is sliently closed if open.
--
fdup2 :: HasCallStack => FD -> FD -> IO FD
fdup2 fd fdto = do
    -- Windows' dup2 does not return the new descriptor, unlike Unix
    throwErrnoIfMinus1 callStack "dup device" $
        c_dup2 fd fdto
    return fdto -- original FD, with the new fdFD

-- | Is 'FD' seekable?
--
fseekable :: FD -> IO Bool
fseekable fd = do
  t <- fdeviceType fd
  return (t == RegularFile || t == RawDevice)

-- | Seeking 'FD'.
fseek :: HasCallStack => String -> FD -> SeekMode -> Int -> IO ()
fseek dev fd mode off = throwErrnoIfMinus1Retry_ callStack dev $
    c_lseek fd (fromIntegral off) seektype
  where
    seektype :: CInt
    seektype = case mode of
        AbsoluteSeek -> sEEK_SET
        RelativeSeek -> sEEK_CUR
        SeekFromEnd  -> sEEK_END

-- | Tell 'FD' current offset.
--
ftell :: HasCallStack => String -> FD -> IO Int
ftell dev fd = fromIntegral `fmap`
    (throwErrnoIfMinus1Retry callStack dev $
        c_lseek fd 0 sEEK_CUR)

-- | Get file size.
--
fgetSize :: HasCallStack => String -> FD -> IO Int
fgetSize dev fd = do
    allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry_ callStack dev $
            c_fstat fd p_stat
        c_mode <- st_mode p_stat :: IO CMode
        if not (s_isreg c_mode)
        then return (-1)
        else do
            c_size <- st_size p_stat
            return (fromIntegral c_size)

-- | Set file size.
--
fsetSize :: HasCallStack => String -> FD -> Int -> IO ()
fsetSize dev fd size = throwErrnoIf_ (/=0) callStack dev $
    c_ftruncate fd (fromIntegral size)

-- | Flush OS cache into disk.
--
fsync :: HasCallStack => String -> FD -> IO ()
fsync dev fd = do
#ifdef mingw32_HOST_OS
    success <- c_FlushFileBuffers =<< c_get_osfhandle fd
    if success
    then return ()
    else do
        err_code <- c_GetLastError
        throwIO $ mkIOError (dev ++ ", error code is 0x" ++ showHex err_code)
                    Nothing Nothing
#else
    throwErrnoIfMinus1_ callStack dev (c_fsync fd)
#endif

--------------------------------------------------------------------------------


-- | Is this 'FD' a pointer?
--
fterminal :: FD -> IO Bool
fterminal fd =
#if defined(mingw32_HOST_OS)
    is_console fd >>= return . toBool
#else
    c_isatty fd >>= return . toBool
#endif

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
fsetCanonical fd cooked =
#ifdef mingw32_HOST_OS
  x <- set_console_buffering fd (if cooked then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setCooked" "failed to set buffering")
   else return ()
#else
  tcSetAttr fd $ \ p_tios -> do

    -- turn on/off ICANON
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag | cooked    = lflag .|. (fromIntegral const_icanon)
                  | otherwise = lflag .&. complement (fromIntegral const_icanon)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when (not cooked) $ do
            c_cc <- ptr_c_cc p_tios
            let vmin  = (c_cc `plusPtr` (fromIntegral const_vmin))  :: Ptr Word8
                vtime = (c_cc `plusPtr` (fromIntegral const_vtime)) :: Ptr Word8
            poke vmin  1
            poke vtime 0
#endif

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

foreign import WINDOWS_CCONV unsafe "HsBase.h closesocket"
   c_closesocket :: CInt -> IO CInt
#else
foreign import capi safe "unistd.h fsync"
    c_fsync :: CInt  -> IO CInt

foreign import ccall unsafe "fdReady"
    unsafe_fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
#endif
