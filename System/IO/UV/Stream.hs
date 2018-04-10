{-# LANGUAGE MultiWayIf #-}

module System.IO.UV.Stream where

import System.IO.UV.Manager
import System.IO.Exception
import System.IO.UV.Exception (uV_EOF)
import System.IO.Buffered
import System.IO.UV.Internal
import Foreign.Ptr
import Foreign.C.Types
import Control.Concurrent.MVar

data UVStream = UVStream
    { uvsHandle     :: Ptr UVHandle
    , uvsReadSlot   :: UVSlot
    , uvsWriteReq   :: Ptr UVReq
    , uvsWriteSlot  :: UVSlot
    , uvsManager    :: UVManager
    }

uvReadStart :: Ptr UVHandle -> IO ()
uvReadStart = throwUVIfMinus_ . hs_uv_read_start
foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt

instance Input UVStream where
    -- readInput :: HasCallStack => UVStream -> Ptr Word8 ->  Int -> IO Int
    readInput uvs@(UVStream handle rslot _ _ uvm) buf len = do
        m <- getBlockMVar uvm rslot
        tryTakeMVar m
        withUVManager' uvm $ do
            pokeBufferTable uvm rslot buf len
            uvReadStart handle
        r <- takeMVar m
        if  | r > 0  -> return r
            -- r == 0 should be impossible, since we guard this situation in c side, but we handle it anyway
            -- nread might be 0, which does not indicate an error or EOF. This is equivalent to EAGAIN or EWOULDBLOCK under read(2)
            | r == 0 -> readInput uvs buf len
            | r == fromIntegral uV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)

uvWrite :: Ptr UVReq -> Ptr UVHandle -> IO ()
uvWrite req handle = throwUVIfMinus_ $ hs_uv_write req handle
foreign import ccall unsafe hs_uv_write :: Ptr UVReq -> Ptr UVHandle -> IO CInt

instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    writeOutput (UVStream handle _ req wslot uvm) buf len = do
        m <- getBlockMVar uvm wslot
        tryTakeMVar m
        withUVManager' uvm $ do
            pokeBufferTable uvm wslot buf len
            uvWrite req handle
        throwUVIfMinus_ $ takeMVar m
