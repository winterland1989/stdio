module System.IO.FD where


import Control.Concurrent.MVar


-- SeekMode type

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
              | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the current position.
              | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the end of the file.
    deriving (Eq, Ord, Enum, Read, Show)

class DiskFD f where
    fseek    :: f -> SeekMode -> Int64 -> IO Int64
    fgetSize :: f -> IO Int64
    fsetSize :: f -> Int64 -> IO ()
    fsync :: f -> IO ()

class FD f where
    type Path f

    fopen :: Path f -> IO f
    fclose :: f -> IO ()

    fread :: f -> Int -> Ptr Word8 -> IO Int
    fwrite :: f -> Ptr Word8 -> Int -> IO ()
    fflush :: f -> IO ()

instance FD File where
instance FD MMap where
instance FD Socket where


