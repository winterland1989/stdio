{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

module System.IO.File where

import qualified Control.Exception as E
import qualified GHC.IO.Exception as E
import System.Posix.Internals
import System.Posix.Types (CDev, CIno)
import Foreign.C
import Foreign.Marshal
import GHC.IO.FD
import Foreign.C.Types
import Data.Int
import Data.Word
import Data.Bits ((.|.))
import GHC.Conc.IO (closeFdWith)
import Numeric (showHex)

data File = File { fileFd :: CInt, filePath :: FilePath }

-- | OS provides seperated flag for file operations(O_CREAT, O_APPEND, O_WRONLY..), but we
-- only provide several combination here for convenience. Notably:
--
-- * 'WriteMode', 'WriteMode' and 'WriteMode' will create file if does not exist.
-- * 'WriteMode' will truncate existing file to empty when opened.
--
data AccessMode
    = ReadMode      -- ^ O_RDONLY
    | WriteMode     -- ^ O_CREAT O_TRUNC O_WRONLY
    | AppendMode    -- ^ O_CREAT O_APPEND O_WRONLY
    | ReadWriteMode -- ^ O_CREAT O_RDWR
  deriving (Eq, Ord, Enum, Read, Show)

-- | Open a 'File', which must be a regular file, otherwise an 'IOError' will be thrown.
--
-- The GHC rts provide file lock so that a file can not be opened multiple times.
--
open :: FilePath -> AccessMode -> IO File
open path mode = withFilePath path $ \ f -> do

    fd <- throwErrnoIfMinus1Retry "System.IO.File.open" (c_open f oflags 0o666)

    (isRegularFile, dev, ino) <- (`E.onException` c_close fd) $ allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry "c_fstat" $ do
            c_fstat fd p_stat
        c_mode <- st_mode p_stat
        dev <- st_dev p_stat
        ino <- st_ino p_stat
        return (s_isreg c_mode, dev, ino)

    if isRegularFile
    then do
        (unique_dev, unique_ino) <- getUniqueFileInfo fd dev ino
        let write = case mode of ReadMode -> False
                                 _ -> True
        r <- lockFile fd unique_dev unique_ino (fromBool write)
        if (r == -1)
        then E.ioException $
            E.IOError Nothing E.ResourceBusy "fopen" "file is locked" Nothing (Just path)
        else return (File fd path)
    else E.ioException $
        E.IOError Nothing E.InappropriateType "fopen" "not a regular file" Nothing (Just path)

  where
#ifdef mingw32_HOST_OS
    commonFLAG = o_BINARY .|. o_NONBLOCK .|. o_NOCTTY
#else
    commonFLAG = o_NONBLOCK .|. o_NOCTTY
#endif
    oflags = case mode of
        ReadMode ->      commonFLAG .|. o_RDONLY
        WriteMode ->     commonFLAG .|. o_CREAT .|. o_TRUNC .|. o_WRONLY
        AppendMode ->    commonFLAG .|. o_CREAT .|. o_APPEND .|. o_WRONLY
        ReadWriteMode -> commonFLAG .|. o_CREAT .|. o_RDWR

-- | Close a 'File'
--
close :: File -> IO ()
close (File fd path) = do
    -- release the lock *first*, because otherwise if we're preempted
    -- after closing but before releasing, the FD may have been reused.
    unlockFile fd
    closeFdWith closer (fromIntegral fd)
  where
    closer realFd = throwErrnoIfMinus1Retry_ "System.IO.File.close" $
         c_close (fromIntegral realFd)

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
              | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the current position.
              | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the end of the file.
    deriving (Eq, Ord, Enum, Read, Show)

-- |
fseek :: File -> SeekMode -> Int64 -> IO Int64
fseek (File fd _) mode off = do
    off' <- throwErrnoIfMinus1Retry "seek" $
        c_lseek fd (fromIntegral off) seektype
    return (fromIntegral off')
    where
        seektype = case mode of
            AbsoluteSeek -> sEEK_SET
            RelativeSeek -> sEEK_CUR
            SeekFromEnd  -> sEEK_END
        fgetSize (File fd _) =
            allocaBytes sizeof_stat $ \ p_stat -> do
                throwErrnoIfMinus1Retry_ "fileSize" $ c_fstat fd p_stat
                fromIntegral `fmap` st_size p_stat  -- regular files always have size

fgetSize :: File -> IO Int64
fgetSize (File fd _) =
    allocaBytes sizeof_stat $ \ p_stat -> do
        throwErrnoIfMinus1Retry_ "fileSize" $ c_fstat fd p_stat
        fromIntegral `fmap` st_size p_stat -- File is guaranteed to be a regular file

fsetSize :: File -> Int64 -> IO ()
fsetSize (File fd _) size = throwErrnoIf_ (/=0) "System.IO.File.fsetSize" $
     c_ftruncate fd (fromIntegral size)

fsync :: File -> IO ()
fsync (File fd fp) = do
#ifdef mingw32_HOST_OS
    success <- c_FlushFileBuffers =<< c_get_osfhandle
    if success
    then return ()
    else do
        err_code <- c_GetLastError
        throwIO $ mkIOError ("System.IO.File.fsync: error code is 0x" ++ showHex err_code)
                    Nothing (Just fp)
#else
    throwErrnoIfMinus1_ "fileSynchronise" (c_fsync fd)
#endif

{-
fopenMode :: File -> AccessMode -> f
fgetMode :: File -> IO AccessMode
-}

--------------------------------------------------------------------------------

foreign import ccall unsafe "lockFile"  -- provide by rts
    lockFile :: CInt -> Word64 -> Word64 -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile" -- provide by rts
    unlockFile :: CInt -> IO CInt

getUniqueFileInfo :: CInt -> CDev -> CIno -> IO (Word64, Word64)
#ifdef mingw32_HOST_OS
getUniqueFileInfo fd _ _ = do
  with 0 $ \devptr -> do
    with 0 $ \inoptr -> do
      c_getUniqueFileInfo fd devptr inoptr
      liftM2 (,) (peek devptr) (peek inoptr)
#else
getUniqueFileInfo _ dev ino = return (fromIntegral dev, fromIntegral ino)
#endif

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "get_unique_file_info"
    c_getUniqueFileInfo :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

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
