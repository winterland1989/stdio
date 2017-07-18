{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : System.IO.File
Description : Regular file operations
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide operations for regular file in platform independent way.

-}

module System.IO.File where

import qualified Control.Exception as E
import qualified System.IO.Exception as E
import Control.Monad.Managed
import GHC.Conc.IO
import System.Posix.Internals hiding (FD)
import System.Posix.Types (CDev, CIno)
import System.IO.Handle (Input(..), Output(..))
import Foreign.Marshal
import Foreign.Ptr
import Foreign.C
import Foreign.C.Types
import Data.Int
import Data.Word
import Data.Bits ((.|.))
import GHC.Stack

data File = File
    { fileFd :: {-# UNPACK #-} !CInt  -- ^ the file descriptor
    , filePath :: FilePath
    }

instance Input File where
    input (File fd path) buf len = do
#ifdef mingw32_HOST_OS
        if rtsSupportsBoundThreads
        then blockingReadRawBufferPtr callStack dev fd buf 0 len
        else asyncReadRawBufferPtr    callStack dev fd buf 0 len
#else
        unsafe_read  -- regular file read will never block
      where
        do_read call = fromIntegral `fmap`
                          throwErrnoIfMinus1RetryMayBlock callStack path call
                                (threadWaitRead (fromIntegral fd))
        unsafe_read = do_read (c_read fd buf (fromIntegral len))
#endif

instance Output File where
    output input (File fd path) buf len = do



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

-- SeekMode type

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode
  = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
  | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                        -- from the current position.
  | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                        -- from the end of the file.
    deriving (Eq, Ord, Enum, Read, Show)

-- | Open a 'File', which must be a regular file, otherwise an 'E.InappropriateType' will be thrown.
--
-- `openFile` locks the file according to the Haskell 2010 single writer/multiple reader
-- locking semantics, throw 'E.ResourceBusy' if locking failed.
--
openFile :: HasCallStack => FilePath -> AccessMode -> Managed File
openFile fp mode = managed (E.bracket (open fp mode) closeFile)
  where
    open :: FilePath -> AccessMode -> IO File  -- inside mask only
    open path mode =
        withFilePath path $ \ f -> do
            fd <- E.throwErrnoIfMinus1Retry callStack fp (c_open f oflags 0o666)

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
                then do
                    c_close fd
                    E.throwIO $ E.ResourceBusy (E.IOEInfo Nothing "file is locked" path callStack)
                else return (File fd path)
            else do
                c_close fd
                E.throwIO $ E.InappropriateType (E.IOEInfo Nothing "not a regular file" path callStack)

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

    closeFile :: File -> IO ()
    closeFile (File fd path) = do
        let closer fd =
                E.throwErrnoIfMinus1Retry_ callStack path $ c_close (fromIntegral fd)
        -- release the lock *first*, because otherwise if we're preempted
        -- after closing but before releasing, the FD may have been reused.
        unlockFile fd
        closeFdWith closer (fromIntegral fd)

-- |
--
seekFile :: HasCallStack => File -> SeekMode -> Int -> IO ()
seekFile (File fd path) mode offset =
    E.throwErrnoIfMinus1Retry_ callStack path $
        c_lseek fd (fromIntegral offset) seektype
  where
    seektype :: CInt
    seektype = case mode of
        AbsoluteSeek -> sEEK_SET
        RelativeSeek -> sEEK_CUR
        SeekFromEnd  -> sEEK_END

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
