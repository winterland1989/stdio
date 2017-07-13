{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

module System.IO.File where

import qualified Control.Exception as E
import qualified GHC.IO.Exception as E
import System.Posix.Internals hiding (FD)
import System.Posix.Types (CDev, CIno)
import System.IO.FD
import Foreign.Marshal
import Foreign.Ptr
import Foreign.C
import Foreign.C.Types
import Data.Int
import Data.Word
import Data.Bits ((.|.))

data File = File { fileFd :: {-# UNPACK #-} !FD, filePath :: FilePath }

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
-- `openFile` locks the file according to the Haskell 2010 single writer/multiple reader
-- locking semantics.
--
openFile :: FilePath -> AccessMode -> IO File
openFile path mode = withFilePath path $ \ f -> do
    let msg = "System.IO.File.open" ++ path

    fd <- throwErrnoIfMinus1Retry msg (c_open f oflags 0o666)

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
            E.IOError Nothing E.ResourceBusy msg "file is locked" Nothing (Just path)
        else return (File fd path)
    else E.ioException $
        E.IOError Nothing E.InappropriateType msg "not a regular file" Nothing (Just path)
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

-- | Release the file lock and close a 'File'
--
closeFile :: File -> IO ()
closeFile (File fd path) = do
    -- release the lock *first*, because otherwise if we're preempted
    -- after closing but before releasing, the FD may have been reused.
    unlockFile fd
    fclose ("System.IO.File.close:" ++ path) fd

-- |
--
seekFile :: File -> SeekMode -> Int -> IO ()
seekFile (File fd path) mode offset =
    fseek ("System.IO.File.seekFile:" ++ path) fd mode offset

{-
getFileSize :: File -> IO Int
getFileSize = fgetSize . fdFD

fsetSize :: File -> Int64 -> IO ()
fsetSize (File fd _) size = throwErrnoIf_ (/=0) "System.IO.File.fsetSize" $



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

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "get_unique_file_info"
    c_getUniqueFileInfo :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
#endif
