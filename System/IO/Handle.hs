{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

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

module System.IO.Handle where


import qualified Control.Exception as E
import qualified System.IO.Exception as E
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ST
import GHC.Prim
import Foreign.Ptr
import Data.Word
import Data.IORef
import Data.Typeable
import qualified Data.Array as A
import qualified Data.Primitive.PrimArray as A
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.UTF8Codec as T

import GHC.Stack.Compat

-- | Input device
--
-- Convention: 'input' should return 0 on EOF.
--
class Show i => Input i where
    readInput :: HasCallStack => i -> Ptr Word8 ->  Int -> IO Int

class Input i => InputWait i where
    readInputWithin :: HasCallStack
                    => i
                    -> Int           -- ^ the timeout
                    -> Int           -- ^ the buffer length
                    -> Ptr Word8     -- ^ the buffer pointer
                    -> IO Int        -- ^ the bytes read

-- | Output device
--
class Show o => Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()

class Output o => OutputWait o where
    outputWithin :: HasCallStack
                 => o
                 -> Ptr Word8
                 -> Int
                 -> IO ()

data InputHandle i = InputHandle
    { bufInput  :: i
    , bufSize  :: {-# UNPACK #-} !Int
    , bufPushBack :: {-# UNPACK #-} !(IORef V.Bytes)
    , spareBuf :: {-# UNPACK #-} !(IORef (A.MutablePrimArray RealWorld Word8))
    }

data OutputHandle o = OutputHandle
    { bufferOutout  :: o
    , bufferIndex   :: {-# UNPACK #-} !Int
    , outputBuffer  :: {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)
    }

newInputHandle :: input -> Int -> IO (InputHandle input)
newInputHandle i bufSiz = do
    pb <- newIORef V.empty
    embuf <- A.newArr 0
    spareBuf <- newIORef embuf
    return (InputHandle i bufSiz pb spareBuf)


-- | Request bytes from 'InputHandle'.
--
-- The buffering logic is quite simple:
--
-- If we have pushed back bytes, directly return it, otherwise we read using buffer size.
-- If we read N bytes, and N is larger than half of the buffer size, then we freeze buffer and return,
-- otherwise we copy buffer into result and reuse buffer afterward.
--
readHandle :: (HasCallStack, Input i) => InputHandle i -> IO V.Bytes
readHandle InputHandle{..} = do
    pb <- readIORef bufPushBack
    if V.null pb
    then do
        sbuf <- readIORef spareBuf
        sbufSiz <- A.sizeofMutableArr sbuf
        buf <- if sbufSiz == 0                 -- spare bufffer is empty
            then A.newPinnedPrimArray bufSize  -- create a new one
            else return sbuf
        l <- readInput bufInput (A.mutablePrimArrayContents buf) bufSize
        if l < bufSize `quot` 2                -- read less than half size
        then do
            mba <- A.newArr l                   -- copy result into new array
            A.copyMutableArr mba 0 buf 0 l
            ba <- A.unsafeFreezeArr mba
            writeIORef spareBuf buf
            return $! V.fromArr ba 0 l
        else do                                 -- freeze buf into result
            when (sbufSiz /= 0) $ do
                embuf <- A.newArr 0
                writeIORef spareBuf embuf
            ba <- A.unsafeFreezeArr buf
            return $! V.fromArr ba 0 l
    else do
        writeIORef bufPushBack V.empty
        return pb

-- | Read exactly N bytes
--
-- If EOF reached before N bytes read, a 'ShortReadException' will be thrown
--
readExactly :: (HasCallStack, Input i) => Int -> InputHandle i -> IO V.Bytes
readExactly n h = V.concat `fmap` (go h n)
  where
    go h n = do
        chunk <- readHandle h
        let l = V.length chunk
        if l > n
        then do
            let (lastChunk, rest) = V.splitAt n chunk
            unReadHandle rest h
            return [lastChunk]
        else if l == n
            then return [chunk]
            else if l == 0
                then
                    E.throwIO (ShortReadException
                        (E.IOEInfo "" "unexpected EOF reached" (show $ bufInput h) callStack))
                else do
                    chunks <- go h (n - l)
                    return (chunk : chunks)

data ShortReadException = ShortReadException E.IOEInfo deriving (Show, Typeable)

instance E.Exception ShortReadException where
    toException = E.ioExceptionToException
    fromException = E.ioExceptionFromException


-- | Push bytes back into handle
--
unReadHandle :: (HasCallStack, Input i) => V.Bytes -> InputHandle i -> IO ()
unReadHandle pb' InputHandle{..} = do
    modifyIORef' bufPushBack $ \ pb -> pb' `V.append` pb

-- | Read until reach a magic bytes
--
-- If EOF reached before meet a magic byte, a 'ShortReadException' will be thrown.

readToMagic :: (HasCallStack, Input i) => Word8 -> InputHandle i -> IO V.Bytes
readToMagic magic h = V.concat `fmap` (go h magic)
  where
    go h magic = do
        chunk <- readHandle h
        if V.null chunk
        then E.throwIO (ShortReadException
            (E.IOEInfo "" "unexpected EOF reached" (show $ bufInput h) callStack))
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadHandle rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)

-- | Read a line
--
readLine :: (HasCallStack, Input i) => InputHandle i -> IO T.Text
readLine h = do
    bss <- go h (V.c2w '\n')
    case T.validateUTF8 (V.concat bss) of
        T.Success t -> return t
        T.PartialBytes _ bs ->
            E.throwIO (UTF8PartialBytesException bs
                (E.IOEInfo "" "utf8 decode error" (show $ bufInput h) callStack))
        T.InvalidBytes bs ->
            E.throwIO (UTF8InvalidBytesException bs
                (E.IOEInfo "" "utf8 decode error" (show $ bufInput h) callStack))
  where
    go h magic = do
        chunk <- readHandle h
        if V.null chunk
        then return []
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadHandle rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)


-- | Read a chunk of text
--
readTextChunk :: (HasCallStack, Input i) => InputHandle i -> IO T.Text
readTextChunk h@InputHandle{..} = do
    chunk <- readHandle h
    if V.null chunk
    then return T.empty
    else do
        let (V.PrimVector vba vs vl) = chunk
            minLen = T.decodeCharLen vba vs
        if minLen > vl                              -- is this chunk partial?
        then do                                     -- if so, try continue reading first
            sbuf <- readIORef spareBuf
            sbufSiz <- A.sizeofMutableArr sbuf
            buf <- if sbufSiz == 0                  -- spare bufffer is empty
                then A.newPinnedPrimArray bufSize   -- create a new one
                else return sbuf
            A.copyArr buf 0 vba vs vl               -- copy the partial chunk into buffer and try read new bytes
            l <- readInput bufInput (A.mutablePrimArrayContents buf `plusPtr` vl) (bufSize - vl)
            let l' = l + vl
            if l' < bufSize `quot` 2                -- read less than half size
            then if l' == vl
                then do                             -- no new bytes read, partial before EOF
                    E.throwIO (UTF8PartialBytesException chunk
                        (E.IOEInfo "" "utf8 decode error" (show $ bufInput) callStack))
                else do
                    mba <- A.newArr l'              -- copy result into new array
                    A.copyMutableArr mba 0 buf 0 l'
                    ba <- A.unsafeFreezeArr mba
                    writeIORef spareBuf buf
                    decode (V.fromArr ba 0 l')
            else do                                 -- freeze buf into result
                when (sbufSiz /= 0) $ do
                    embuf <- A.newArr 0
                    writeIORef spareBuf embuf
                ba <- A.unsafeFreezeArr buf
                decode (V.fromArr ba 0 l')
        else decode chunk
  where
    decode bs = case T.validateUTF8 bs of
        T.Success t -> return t
        T.PartialBytes t bs -> do
            unReadHandle bs h
            return t
        T.InvalidBytes bs -> E.throwIO (UTF8InvalidBytesException bs
                (E.IOEInfo "" "utf8 decode error" (show $ bufInput) callStack))


data UTF8DecodeException
    = UTF8InvalidBytesException V.Bytes E.IOEInfo
    | UTF8PartialBytesException V.Bytes E.IOEInfo
  deriving (Show, Typeable)
instance E.Exception UTF8DecodeException where
    toException = E.ioExceptionToException
    fromException = E.ioExceptionFromException


{-
-- | Write 'V.Bytes' into buffered handle.
--
-- Copy 'V.Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'V.Bytes'.
--
write :: (Output o) => OutputHandle o -> V.Bytes -> IO ()
write OutputHandle{..} (V.PrimVector ba s l) = do
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
    go :: (Output o) => o -> Ptr a -> A.PrimArray Word8 -> Int -> IO (Int, ())
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
flush :: Output f => OutputHandle f -> IO ()
flush OutputHandle{..} =
    when (wBufLen > 0) (hLoopWrite handle writeBuf writeBufSize)
    return (0, ())
-}
