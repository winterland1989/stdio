{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}


{-|
Module      : System.IO.Buffered
Description : Buffered I/O interface
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide basic I/O interface stdio use.

-}

module System.IO.Buffered where


import Control.Concurrent.MVar
import Control.Monad
import GHC.Stack
import GHC.Prim
import Foreign.Ptr
import Data.Word
import Data.Array
import Data.Text
import Data.Vector

-- | Input device
--
-- Convention: 'input' should return 0 on EOF.
--
class Input i where
    readInput :: HasCallStack => i -> Int -> Ptr Word8 -> IO Int

class Input i => InputWait i where
    readInputWithin :: HasCallStack
                    => i
                    -> Int           -- ^ the timeout
                    -> Int           -- ^ the buffer length
                    -> Ptr Word8     -- ^ the buffer pointer
                    -> IO Int        -- ^ the bytes read

-- | Output device
--
class Output o where
    output :: HasCallStack => o -> Ptr Word8 -> Int -> IO Int

class Output o => OutputWait o where
    outputWithin :: HasCallStack
                 => o
                 -> Ptr Word8
                 -> Int
                 -> IO Int

data BufferedInput i = BufferedInput
    { bufInput  :: i
    , bufOffset :: {-# UNPACK #-} !Int
    , bufLength :: {-# UNPACK #-} !Int
    , inputBuf  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
    }

data BufferedOutput o = BufferedOutput
    { bufferOutout  :: o
    , bufferIndex   :: {-# UNPACK #-} !Int
    , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
    }

newBufferedInput :: input -> Int -> IO (BufferedInput input)
newBufferedInput i bufSiz = do
    buf <- newArr bufSiz
    return (BufferedInput_ i 0 0 buf)


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
readExactly :: (HasCallStack, Input i) => BufferedInput i -> Int -> IO Bytes
readExactly biLock len = modifyMVar biLock $ \ bi@BufferedInput_{..} -> do
    bufSiz <- sizeofMutableArr inputBuf
    if len <= bufLength
    then
        if bufLength < bufSiz `quot` 2
        then do
            resultBuf <- newArr len                             -- no need to pinned here
            copyMutableArr resultBuf 0 inputBuf bufOffset len
            result <- unsafeFreezeArr resultBuf
            return (BufferedInput_ bufInput (bufOffset+len) (bufLength-len) inputBuf, fromArr result 0 len)
        else do
            newBuf <- newPinnedPrimArray bufSiz
            result <- unsafeFreezeArr inputBuf
            return (BufferedInput_ bufInput 0 0 newBuf, fromArr result bufOffset bufLength)

    else
        if len - bufLength < bufSiz `quot` 2
        then do
            resultBuf <- newArr len
            loopRead1 resultBuf bi 0 len
        else do
            resultBuf <- newPinnedPrimArray len
            loopRead2 resultBuf bi 0 len
  where
    loopRead1 :: Input i => MutablePrimArray RealWorld Word8 -> BufferedInput_ i -> Int -> Int -> IO (BufferedInput_ i, Bytes)
    loopRead1 resultBuf bi@BufferedInput_{..} i len
        | i == len = do
            result <- unsafeFreezeArr resultBuf
            return (bi, fromArr result 0 len)
        | otherwise = do
            when (bufOffset /= 0 && bufLength /=0) (moveArr inputBuf 0 inputBuf bufOffset bufLength) -- slide buffer
            bufSiz <- sizeofMutableArr inputBuf
            l <- input bufInput bufSiz (mutablePrimArrayContents inputBuf `plusPtr` bufLength)  -- read to old buffer
            let l' = min (len-i) (bufLength + l)
            copyMutableArr resultBuf i inputBuf 0 l'
            loopRead1 resultBuf (BufferedInput_ bufInput l' (bufLength+l-l') inputBuf) (i+l') len

    loopRead2 :: Input i => MutablePrimArray RealWorld Word8 -> BufferedInput_ i -> Int -> Int -> IO (BufferedInput_ i, Bytes)
    loopRead2 resultBuf bi@BufferedInput_{..} i len
        | i == len = do
            result <- unsafeFreezeArr resultBuf
            return (bi, fromArr result 0 len)
        | otherwise = do
            if len - i


readChunk :: (HasCallStack, Input i) => BufferedInput i -> (Bytes -> (a, Bytes)) -> IO a
readChunk = undefined

bufInputText :: Input i => BufferedInput i -> IO Text
bufInputText = undefined

-- | Write 'Bytes' into buffered handle.
--
-- Copy 'Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'Bytes'.
--
write :: (Output o) => BufferedOutput o -> Bytes -> IO ()
write output (PrimVector ba s l) = modifyMVar output $ \ BufferedOutput_{..} ->
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
    go :: (Output o) => o -> Ptr a -> PrimArray Word8 -> Int -> IO (Int, ())
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
flush :: Output f => BufferedOutput f -> IO ()
flush output = modifyMVar output $ \ BufferedOutput_{..} ->
    when (wBufLen > 0) (hLoopWrite handle writeBuf writeBufSize)
    return (0, ())
