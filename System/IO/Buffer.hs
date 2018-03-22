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

module System.IO.Buffer where


import System.IO.Exception
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ST
import GHC.Prim
import Foreign.Ptr
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
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
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 ->  Int -> IO Int

-- | Output device
--
class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()

data InputBuffer i = InputBuffer
    { bufInput    :: i
    , bufSize     :: {-# UNPACK #-} !Int
    , bufPushBack :: {-# UNPACK #-} !(IORef V.Bytes)
    , inputBuffer :: {-# UNPACK #-} !(IORef (A.MutablePrimArray RealWorld Word8))
    }

data OutputBuffer o = OutputBuffer
    { bufOutput     :: o
    , bufIndex      :: {-# UNPACK #-} !(IORefU Int)
    , outputBuffer  :: {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)
    }

newInputBuffer :: input -> Int -> IO (InputBuffer input)
newInputBuffer i bufSiz = do
    pb <- newIORef V.empty
    embuf <- A.newArr 0
    inputBuffer <- newIORef embuf
    return (InputBuffer i bufSiz pb inputBuffer)


-- | Request bytes from 'InputBuffer'.
--
-- The buffering logic is quite simple:
--
-- If we have pushed back bytes, directly return it, otherwise we read using buffer size.
-- If we read N bytes, and N is larger than half of the buffer size, then we freeze buffer and return,
-- otherwise we copy buffer into result and reuse buffer afterward.
--
readBuffer :: (HasCallStack, Input i) => InputBuffer i -> IO V.Bytes
readBuffer InputBuffer{..} = do
    pb <- readIORef bufPushBack
    if V.null pb
    then do
        rbuf <- readIORef inputBuffer
        bufSiz <- A.sizeofMutableArr rbuf
        buf <- if bufSiz == 0                 -- spare bufffer is empty
            then A.newPinnedPrimArray bufSize  -- create a new one
            else return rbuf
        l <- readInput bufInput (A.mutablePrimArrayContents buf) bufSize
        if l < bufSize `quot` 2                -- read less than half size
        then do
            mba <- A.newArr l                   -- copy result into new array
            A.copyMutableArr mba 0 buf 0 l
            ba <- A.unsafeFreezeArr mba
            writeIORef inputBuffer buf
            return $! V.fromArr ba 0 l
        else do                                 -- freeze buf into result
            when (bufSiz /= 0) $ do
                embuf <- A.newArr 0
                writeIORef inputBuffer embuf
            ba <- A.unsafeFreezeArr buf
            return $! V.fromArr ba 0 l
    else do
        writeIORef bufPushBack V.empty
        return pb

-- | Read exactly N bytes
--
-- If EOF reached before N bytes read, a 'ShortReadException' will be thrown
--
readExactly :: (HasCallStack, Input i) => Int -> InputBuffer i -> IO V.Bytes
readExactly n h = V.concat `fmap` (go h n)
  where
    go h n = do
        chunk <- readBuffer h
        let l = V.length chunk
        if l > n
        then do
            let (lastChunk, rest) = V.splitAt n chunk
            unReadBuffer rest h
            return [lastChunk]
        else if l == n
            then return [chunk]
            else if l == 0
                then
                    throwIO (ShortReadException
                        (IOEInfo "" "unexpected EOF reached" callStack))
                else do
                    chunks <- go h (n - l)
                    return (chunk : chunks)

data ShortReadException = ShortReadException IOEInfo deriving (Show, Typeable)

instance Exception ShortReadException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException


-- | Push bytes back into buffer
--
unReadBuffer :: (HasCallStack, Input i) => V.Bytes -> InputBuffer i -> IO ()
unReadBuffer pb' InputBuffer{..} = do
    modifyIORef' bufPushBack $ \ pb -> pb' `V.append` pb

-- | Read until reach a magic bytes
--
-- If EOF reached before meet a magic byte, a 'ShortReadException' will be thrown.

readToMagic :: (HasCallStack, Input i) => Word8 -> InputBuffer i -> IO V.Bytes
readToMagic magic h = V.concat `fmap` (go h magic)
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then throwIO (ShortReadException
            (IOEInfo "" "unexpected EOF reached" callStack))
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)

-- | Read a line
--
readLine :: (HasCallStack, Input i) => InputBuffer i -> IO T.Text
readLine h = do
    bss <- go h (V.c2w '\n')
    case T.validateUTF8 (V.concat bss) of
        T.Success t -> return t
        T.PartialBytes _ bs ->
            throwIO (UTF8PartialBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))
        T.InvalidBytes bs ->
            throwIO (UTF8InvalidBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then return []
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)


-- | Read a chunk of text
--
readTextChunk :: (HasCallStack, Input i) => InputBuffer i -> IO T.Text
readTextChunk h@InputBuffer{..} = do
    chunk <- readBuffer h
    if V.null chunk
    then return T.empty
    else do
        let (V.PrimVector vba vs vl) = chunk
            minLen = T.decodeCharLen vba vs
        if minLen > vl                              -- is this chunk partial?
        then do                                     -- if so, try continue reading first
            rbuf <- readIORef inputBuffer
            bufSiz <- A.sizeofMutableArr rbuf
            buf <- if bufSiz == 0                  -- spare bufffer is empty
                then A.newPinnedPrimArray bufSize   -- create a new one
                else return rbuf
            A.copyArr buf 0 vba vs vl               -- copy the partial chunk into buffer and try read new bytes
            l <- readInput bufInput (A.mutablePrimArrayContents buf `plusPtr` vl) (bufSize - vl)
            let l' = l + vl
            if l' < bufSize `quot` 2                -- read less than half size
            then if l' == vl
                then do                             -- no new bytes read, partial before EOF
                    throwIO (UTF8PartialBytesException chunk
                        (IOEInfo "" "utf8 decode error" callStack))
                else do
                    mba <- A.newArr l'              -- copy result into new array
                    A.copyMutableArr mba 0 buf 0 l'
                    ba <- A.unsafeFreezeArr mba
                    writeIORef inputBuffer buf
                    decode (V.fromArr ba 0 l')
            else do                                 -- freeze buf into result
                when (bufSiz /= 0) $ do
                    embuf <- A.newArr 0
                    writeIORef inputBuffer embuf
                ba <- A.unsafeFreezeArr buf
                decode (V.fromArr ba 0 l')
        else decode chunk
  where
    decode bs = case T.validateUTF8 bs of
        T.Success t -> return t
        T.PartialBytes t bs -> do
            unReadBuffer bs h
            return t
        T.InvalidBytes bs -> throwIO (UTF8InvalidBytesException bs
                (IOEInfo "" "utf8 decode error" callStack))


data UTF8DecodeException
    = UTF8InvalidBytesException V.Bytes IOEInfo
    | UTF8PartialBytesException V.Bytes IOEInfo
  deriving (Show, Typeable)
instance Exception UTF8DecodeException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

--------------------------------------------------------------------------------


-- | Write 'V.Bytes' into buffered handle.
--
-- Copy 'V.Bytes' to buffer if it can hold, otherwise
-- write both buffer(if not empty) and 'V.Bytes'.
--
writeBuffer :: (Output o) => OutputBuffer o -> V.Bytes -> IO ()
writeBuffer o@OutputBuffer{..} v@(V.PrimVector ba s l) = do
    i <- readIORefU bufIndex
    bufSize <- A.sizeofMutableArr outputBuffer
    if i + l <= bufSize
    then do
        -- current buffer can hold it
        copyByteArray outputBuffer i ba s l     -- copy to buffer
        writeIORefU bufIndex (i+l)              -- update index
    else do
        if (i > 0)
        then do
            -- flush the buffer
            withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput ptr i
            writeIORefU bufIndex 0

            writeBuffer o v -- try write to buffer again
        else
            -- directly write bytes to output
            if isPrimArrayPinned ba
            then do
                -- which is mostly the case, since default buffer size is larger than unpinned limit
                withMutablePrimArrayContents ba $ \ ptr ->
                    writeOutput bufOutput (ptr `addrToPtr` s) l
            else do
                -- this is almost impossible, so we just create an pinned copy and send it
                buf <- newPinnedPrimArray l
                copyPrimArray buf 0 ba s l
                withMutablePrimArrayContents buf $ \ ptr -> writeOutput bufOutput ptr i

-- | Flush the buffer(if not empty).
--
flush :: Output f => OutputBuffer f -> IO ()
flush OutputBuffer{..} = do
    i <- readIORefU bufIndex
    withMutablePrimArrayContents outputBuffer $ \ ptr -> writeOutput bufOutput ptr i
    writeIORefU bufIndex 0
