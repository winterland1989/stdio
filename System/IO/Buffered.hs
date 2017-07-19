{-# LANGUAGE BangPatterns #-}

module System.IO.Buffered where


import Control.Concurrent.MVar
import GHC.Stack
import Foreign.Ptr
import Data.Word
import Data.Array
import Data.Vector

-- | Input device
--
-- Convention: 'input' should return 0 on EOF.
--
class Input i where
    input :: HasCallStack => i -> Int -> Ptr Word8 -> IO Int

class Input i => InputWait i where
    inputWithin :: HasCallStack
                => i
                -> Int
                -> Int
                -> Ptr Word8
                -> IO Int

-- | Output device
--
class Output o where
    output :: o -> Ptr Word8 -> Int -> IO Int

class Output o => OutputWait o where
    outputWithin :: o
                 -> Ptr Word8
                 -> Int
                 -> IO Int

data BufferedInput_ i = BufferedInput_
    { bufferInput  :: i
    , bufferOffset :: {-# UNPACK #-} !Int
    , bufferLength :: {-# UNPACK #-} !Int
    , inputBuffer  :: {-# UNPACK #-} !(MutablePrimArray Word8)
    }

type BufferedInput i = MVar BufferedInput_ i

data BufferedOutput_ o = BufferedOutput_ o
    { bufferOutout  :: o
    , bufferIndex   :: {-# UNPACK #-} !Int
    , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray Word8)
    }

type BufferedOutput o = MVar BufferedOutput_ o


newBufferedInput :: input -> Int -> IO (BufferedInput input)
newBufferedInput i bufSiz = do
    buf <- newArr bufSiz
    newMVar (BufferedInput_ (input i) 0 0 buf)


-- | Request certain N bytes from 'BufferedInput'.
--
--  If Internal buffer reach EOF, it will return less than N bytes.
--
-- The buffering logic is quite subtle:
--
--  * If there's enough bytes in buffer and N < bufferSize/2, copy the bytes from buffer.
--
--  * If there's enough bytes in buffer and N >= bufferSize/2, freeze the old buffer.
--
--  * If there's no enough bytes left, we will allocate a new buffer of size N, and copy first M
--    bytes from old buffer, then
--
--    1. If (N-M) < bufferSize/2, we use old buffer to receive new data, and copy N-M bytes afterwards.
--    2. If (N-M) >= bufferSize/2, we directly use new buffer to receive data.
--    3 .If enough bytes are received, we freeze the new buffer as input, otherwise go back to 1.
--
readExactly :: Input i => BufferedInput i -> Int -> IO Bytes
readExactly input len = modifyMVar input $ \ BufferedInput_{..} -> do
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


readChunk :: BufferedInput -> (Bytes -> (a, Bytes)) -> IO a
readChunk = undefined

bufInputText :: BufferedInput -> IO Text
bufInputText = undefined

-- | Write 'Bytes' into buffered handle.
--
-- Copy 'Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'Bytes'.
--
write :: (FD f) => BufferedOutput o -> Bytes -> IO ()
write BufferedOutput{..} (PrimVector ba s l) = modifyMVar writeBufLen $ \ wBufLen ->
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
flush :: (FD f) => Handle f -> IO ()
flush Hand{..} = modifyMVar writeBufLen $ \ wBufLen ->
    when (wBufLen > 0) (hLoopWrite handle writeBuf writeBufSize)
    return (0, ())


seek :: (DiskFD f) => Handle f -> SeekMode -> Int -> IO ()
getFileSize :: (DiskFD f) => Handle f -> IO Int
setFileSize :: (DiskFD f) => Handle f -> Int -> IO ()
