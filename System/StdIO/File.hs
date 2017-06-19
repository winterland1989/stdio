{-# LANGUAGE CPP #-}
module System.StdIO.File where

import qualified Control.Exception as E
import qualified GHC.IO.Exception as E
import qualified System.Posix.Internals as C
import System.Posix.Types (CDev, CIno)
import qualified Foreign.C as C
import qualified Foreign.Marshal as C
import qualified GHC.IO.FD as C
import Foreign.C.Types
import Data.Int
import Data.Word
import Data.Bits ((.|.))
import GHC.Conc.IO (closeFdWith)

data File = File { fileFd :: C.CInt, filePath :: FilePath }

data AccessMode
    = ReadMode      -- O_RDONLY
    | WriteMode     -- O_CREAT O_TRUNC O_WRONLY
    | AppendMode    -- O_CREAT O_APPEND O_WRONLY
    | ReadWriteMode -- O_CREAT O_RDWR
  deriving (Eq, Ord, Enum, Read, Show)


open :: FilePath -> AccessMode -> IO File
open path mode = C.withFilePath path $ \ f -> do

    fd <- C.throwErrnoIfMinus1Retry "System.IO.File.open" (C.c_open f oflags 0o666)

    (isRegularFile, dev, ino) <- (`E.onException` C.c_close fd) $ C.allocaBytes C.sizeof_stat $ \ p_stat -> do
        C.throwErrnoIfMinus1Retry "c_fstat" $ do
            C.c_fstat fd p_stat
        c_mode <- C.st_mode p_stat
        dev <- C.st_dev p_stat
        ino <- C.st_ino p_stat
        return (C.s_isreg c_mode, dev, ino)

    if isRegularFile
    then do
        (unique_dev, unique_ino) <- getUniqueFileInfo fd dev ino
        let write = case mode of ReadMode -> False
                                 _ -> True
        r <- lockFile fd unique_dev unique_ino (C.fromBool write)
        if (r == -1)
        then E.ioException $
            E.IOError Nothing E.ResourceBusy "fopen" "file is locked" Nothing (Just path)
        else return (File fd path)
    else E.ioException $
        E.IOError Nothing E.InappropriateType "fopen" "not a regular file" Nothing (Just path)

  where
#ifdef mingw32_HOST_OS
    commonFLAG = C.o_BINARY .|. C.o_NONBLOCK .|. C.o_NOCTTY
#else
    commonFLAG = C.o_NONBLOCK .|. C.o_NOCTTY
#endif
    oflags = case mode of
        ReadMode ->      commonFLAG .|. C.o_RDONLY
        WriteMode ->     commonFLAG .|. C.o_CREAT .|. C.o_TRUNC .|. C.o_WRONLY
        AppendMode ->    commonFLAG .|. C.o_CREAT .|. C.o_APPEND .|. C.o_WRONLY
        ReadWriteMode -> commonFLAG .|. C.o_CREAT .|. C.o_RDWR


close :: File -> IO ()
close (File fd path) = do
    -- release the lock *first*, because otherwise if we're preempted
    -- after closing but before releasing, the FD may have been reused.
    unlockFile fd
    closeFdWith closer (fromIntegral fd)
  where
    closer realFd = C.throwErrnoIfMinus1Retry_ "System.IO.File.close" $
         C.c_close (fromIntegral realFd)

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
              | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the current position.
              | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the end of the file.
    deriving (Eq, Ord, Enum, Read, Show)

fseek :: File -> SeekMode -> Int64 -> IO Int64
fseek (File fd _) mode off = do
    off' <- C.throwErrnoIfMinus1Retry "seek" $
        C.c_lseek fd (fromIntegral off) seektype
    return (fromIntegral off')
    where
        seektype = case mode of
            AbsoluteSeek -> C.sEEK_SET
            RelativeSeek -> C.sEEK_CUR
            SeekFromEnd  -> C.sEEK_END
        fgetSize (File fd _) =
            C.allocaBytes C.sizeof_stat $ \ p_stat -> do
                C.throwErrnoIfMinus1Retry_ "fileSize" $ C.c_fstat fd p_stat
                fromIntegral `fmap` C.st_size p_stat  -- regular files always have size

{-
fgetSize :: File -> IO Int64
fsetSize :: File -> Int64 -> IO ()
fsync :: File -> IO ()
fopenMode :: File -> AccessMode -> f
fgetMode :: File -> IO AccessMode
-}

--------------------------------------------------------------------------------

foreign import ccall unsafe "lockFile"
  lockFile :: CInt -> Word64 -> Word64 -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CInt -> IO CInt

getUniqueFileInfo :: CInt -> CDev -> CIno -> IO (Word64, Word64)
#ifndef mingw32_HOST_OS
getUniqueFileInfo _ dev ino = return (fromIntegral dev, fromIntegral ino)
#else
getUniqueFileInfo fd _ _ = do
  with 0 $ \devptr -> do
    with 0 $ \inoptr -> do
      c_getUniqueFileInfo fd devptr inoptr
      liftM2 (,) (peek devptr) (peek inoptr)
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "get_unique_file_info"
  c_getUniqueFileInfo :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
#endif
