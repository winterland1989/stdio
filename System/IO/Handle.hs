module System.IO.Handle where


import Control.Concurrent.MVar

-- SeekMode type

-- | A mode that determines the effect of 'hSeek' @hdl mode i@.
data SeekMode
  = AbsoluteSeek        -- ^ the position of @hdl@ is set to @i@.
  | RelativeSeek        -- ^ the position of @hdl@ is set to offset @i@
                        -- from the current position.
  | SeekFromEnd         -- ^ the position of @hdl@ is set to offset @i@
                        -- from the end of the file.
    deriving (Eq, Ord, Ix, Enum, Read, Show)

class Seekable h where
    hseek    :: h -> SeekMode -> Int64 -> IO Int64
    hgetSize :: h -> IO Int64
    hsetSize :: h -> Int64 -> IO ()

class Handle h where
    type URI h
    type Config h

    hopen :: URI h -> Config h -> IO h
    hclose :: h -> IO ()

    hread :: h -> Int -> Ptr Word8 -> IO Int
    hwrite :: Ptr Word8 -> Int -> h -> IO ()
    hwritev :: [(Ptr Word8, Int)] -> h -> IO ()

    hsync :: h -> IO ()

data File =

-- | Buffered handles
--
-- A Buffered handle contain a read buffer and write buffer which are protected by seperated 'MVar's.
--
--
-- @
--
--
--
--
--

data Buffered h = Buffered
    { handle   :: !h
    , readBufSize  :: {-# UNPACK #-} !Int
    , writeBufSize :: {-# UNPACK #-} !Int
    , readBuf      :: {-# UNPACK #-} !MVar [Bytes]
    , writeBuf     :: MutableByteArray
    , writeBufLen  :: {-# UNPACK #-} !MVar Int
    }

buffered :: (Handle h) => h -> IO (Buffered h)
buffered h =

read :: (Handle h) => Buffered h -> IO (Maybe Bytes)
read Buffered{..} = modifyMVar readBuf $ \ rbuf ->
    case rbuf of
        (x:xs) -> (xs, x)
        e -> do
            mba <- newPinnedByteArray readBufSize
            r <- read handle readBufSize (mutableByteArrayContents mba)
            if r == 0
                then return (e, Nothing)
                else do
                    ba <- unsafeFreezeByteArray mba
                    return (e, Just (Vector ba 0 0))


pushBack :: (Handle h) => Buffered h -> Bytes -> IO ()
pushBack Buffered{..} x = modifyMVar_ readBuf $ \ rbuf -> return (x:rbuf)


write :: (Handle h) => Buffered h -> Bytes -> IO ()
write Buffered{..} bs = modifyMVar writeBufLen $ \ wbufLen ->
    if wbufLen + B.length bs <= writeBufSize
    then do

    else


writeChunks :: (Handle h) => Buffered h -> [Bytes] -> IO ()

