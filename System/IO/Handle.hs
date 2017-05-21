module System.IO.Handle where


import Control.Concurrent.MVar

-- SeekMode type

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
              | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the current position.
              | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                                    -- from the end of the file.
    deriving (Eq, Ord, Enum, Read, Show)

class SeekableFD f where
    fseek    :: f -> SeekMode -> Int64 -> IO Int64
    fgetSize :: f -> IO Int64
    fsetSize :: f -> Int64 -> IO ()
    fsync :: f -> IO ()

class FD f where
    type URI f
    type Config f

    fopen :: URI f -> Config f -> IO f
    fclose :: f -> IO ()

    fread :: f -> Int -> Ptr Word8 -> IO Int
    fwrite :: f -> Ptr Word8 -> Int -> IO Int

hLoopWrite :: Handle f => f -> Ptr Word8 -> Int -> IO ()


newtype File = File FD

-- | Handle
--
-- A Handle handle contain a read buffer and write buffer which are protected by seperated 'MVar's.
--
--
-- @
--
--
--

data Handle f = Handle
    { fd  :: !f
    , readBufSize  :: {-# UNPACK #-} !Int
    , readBuf      :: {-# UNPACK #-} !MVar [Bytes]
    , writeBufSize :: {-# UNPACK #-} !Int
    , writeBuf     :: {-# UNPACK #-} !(Ptr Word8)
    , writeBufLen  :: {-# UNPACK #-} !MVar Int
    }

newHandle32K :: FD f => f -> IO (Handle f)
newHandle32K f = buffered f bufSiz bufSiz
  where bufSiz = (32 * 1024) - 2 * sizeOf (undefined :: Word)

newHandle :: FD f => f -> Int -> Int -> IO (Handle f)
newHandle f rbufSize wbufSiz =

read :: (FD f) => Handle f -> IO (Maybe Bytes)
read Handle{..} = modifyMVar readBuf $ \ rbuf ->
    case rbuf of
        (x:xs) -> (xs, x)
        e -> do
            mba <- newPinnedByteArray readBufSize
            r <- read handle readBufSize (mutableByteArrayContents mba)
            if r == 0
                then return (e, Nothing)
                else do
                    ba <- unsafeFreezeByteArray mba
                    return (e, Just (PVector ba 0 0))


push :: (FD f) => Handle f -> Bytes -> IO ()
push Handle{..} x = modifyMVar_ readBuf $ \ rBuf -> return (x:rBuf)

-- | Write 'Bytes' into buffered handle.
--
-- Copy 'Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'Bytes'.
--
write :: (FD f) => Handle f -> Bytes -> IO ()
write Handle{..} (PVector ba s l) = modifyMVar writeBufLen $ \ wBufLen ->
    if wBufLen + l <= writeBufSize
    then do
        -- current buffer can hold it
        copyByteArray writeBuf wBufLen ba s l
        let !wBufLen' = (wBufLen + l)
        return (wBufLen', ())
    else do
        -- flush buffer
        when (wBufLen > 0) (hLoopWrite handle writeBuf writeBufSize)
        if isPrimArrayPinned ba
        -- if we have a pinned bytearray, which is most of the cases.
        then do
            hLoopWrite handle (addrToPtr (byteArrayContents ba) `plusPtr` s) l
            return (0, ()) -- The writeBuf is empty now,
        else do
            -- somehow we meet an unpinned bytearray,
            -- we use writeBuf as a transfer buffer.
            go handle writeBuf writeBufSize ba s l
  where
    go :: (FD f) => f -> Ptr a -> PrimArray Word8 -> Int -> IO (Int, ())
    go f wbuf !wbufSiz !ba !s !l
        -- writeBuf is enough to transfer all the bytearray
        | l <= wbufSiz = do
            copyByteArray wbuf 0 ba s l
            hLoopWrite f wbuf l
            return (0, ())
        -- loop to transfer
        | otherwise = do
            copyByteArray wbuf 0 ba s wbufSiz
            hLoopWrite f wbuf wbufSiz
            go f wbuf wbufSiz ba (s+wbufSiz) (l-wbufSiz)

-- | Flush the buffer(if not empty).
--
flush :: (FD f) => Hand f -> IO ()
flush Hand{..} = modifyMVar writeBufLen $ \ wBufLen ->
    when (wBufLen > 0) (hLoopWrite handle writeBuf writeBufSize)
    return (0, ())
