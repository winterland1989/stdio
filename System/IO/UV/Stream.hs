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
import GHC.Conc.Sync (newStablePtrPrimMVar, PrimMVar)
import Control.Monad.IO.Class

data UVStream = UVStream
    { uvsHandle        :: {-# UNPACK #-} !(Ptr UVHandle)
    , uvsHandleContext :: {-# UNPACK #-} !(Ptr UVContext)
    , uvsReadMVar      :: {-# UNPACK #-} !(MVar ())
    , uvsWriteReq      :: {-# UNPACK #-} !(Ptr UVReq)
    , uvsReqContext    :: {-# UNPACK #-} !(Ptr UVContext)
    , uvsWriteMVar     :: {-# UNPACK #-} !(MVar ())
    , uvsManager       :: UVManager
    }

initTCPStream :: HasCallStack => Resource UVStream
initTCPStream = do
    uvm <- liftIO getUVManager
    (handle, rcontext) <- initUVHandle uV_TCP (\ loop handle -> uv_tcp_init loop handle >> return handle) uvm
    (wreq, wcontext) <- initUVReq uV_WRITE
    liftIO $ do
        rmvar <- newEmptyMVar
        wmvar <- newEmptyMVar
        return (UVStream handle rcontext rmvar wreq wcontext wmvar uvm)

instance Input UVStream where
    -- readInput :: HasCallStack => UVStream -> Ptr Word8 ->  Int -> IO Int
    readInput uvs@(UVStream handle rcontext rmvar _ _ _ uvm) buf len = do
        rmvarSP <- newStablePtrPrimMVar rmvar
        pokeUVHandleContextMVar rmvarSP (uvmCap uvm) rcontext
        pokeUVHandleContextBuffer buf len rcontext
        throwUVIfMinus_ . lockUVManager' uvm $ hs_uv_read_start handle
        takeMVar rmvar
        r <- peekUVHandleContextResult rcontext
        if  | r > 0  -> return r
            -- nread might be 0, which does not indicate an error or EOF. This is equivalent to EAGAIN or EWOULDBLOCK under read(2)
            | r == 0 -> readInput uvs buf len
            | r == fromIntegral uV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)

foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt


instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    writeOutput (UVStream handle _ _ wreq wcontext wmvar uvm) buf len = do
        wmvarSP <- newStablePtrPrimMVar wmvar
        pokeUVReqContextMVar wmvarSP (uvmCap uvm) wcontext
        pokeUVReqContextBuffer buf len wcontext
        throwUVIfMinus_ . lockUVManager' uvm $ hs_uv_write wreq handle
        takeMVar wmvar
        throwUVIfMinus_ $ peekUVReqContextResult wcontext
foreign import ccall unsafe hs_uv_write :: Ptr UVReq -> Ptr UVHandle -> IO CInt
