module System.IO.UV.Stream where

import System.IO.UV.Manager
import System.IO.Exception
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
    readInput (UVStream handle rslot _ _ uvm) buf len = do
        m <- getBlockMVar uvm rslot
        tryTakeMVar m
        pokeBufferTable uvm rslot buf len
        withUVManager' uvm $ uvReadStart handle
        throwUVIfMinus $ takeMVar m

uvWrite :: Ptr UVReq -> Ptr UVHandle -> IO ()
uvWrite req handle = throwUVIfMinus_ $ hs_uv_write req handle
foreign import ccall unsafe hs_uv_write :: Ptr UVReq -> Ptr UVHandle -> IO CInt

instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    writeOutput (UVStream handle _ req wslot uvm) buf len = do
        m <- getBlockMVar uvm wslot
        tryTakeMVar m
        pokeBufferTable uvm wslot buf len
        withUVManager' uvm $ uvWrite req handle
        throwUVIfMinus_ $ takeMVar m
