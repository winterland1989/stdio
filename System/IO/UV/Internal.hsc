{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

module System.IO.UV.Internal where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Data.Word
import System.Posix.Types (CSsize(..))
import System.IO.Exception
import Data.Bits
import System.IO.Net.SockAddr (SockAddr, SocketFamily(..))
import GHC.Conc.Sync(PrimMVar)

#include "uv.h"
#include "hs_uv.h"

--------------------------------------------------------------------------------

data UVLoop

newtype UVRunMode = UVRunMode CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVRunMode, UVRunMode, 
  uV_RUN_DEFAULT = UV_RUN_DEFAULT,
  uV_RUN_ONCE    = UV_RUN_ONCE,
  uV_RUN_NOWAIT  = UV_RUN_NOWAIT}

-- | Peek loop data pointer from uv loop  pointer.
peekUVLoopData :: Ptr UVLoop -> IO CSize
peekUVLoopData p = fromIntegral <$> (#{peek uv_loop_t, data} p :: IO CIntPtr)

-- | Poke loop data pointer from uv loop  pointer.
clearUVLoopData :: Ptr UVLoop -> IO ()
clearUVLoopData p = #{poke uv_loop_t, data} p (0 :: CSize)

-- | Initialize an uv loop.
initUVLoop :: HasCallStack => Int -> Resource (Ptr UVLoop)
initUVLoop siz = initResource (throwOOMIfNull $ hs_uv_loop_init (fromIntegral siz)) hs_uv_loop_close
foreign import ccall unsafe hs_uv_loop_init      :: CSize -> IO (Ptr UVLoop)
foreign import ccall unsafe hs_uv_loop_close     :: Ptr UVLoop -> IO ()

-- | uv_run with usafe FFI.
uvRun :: HasCallStack => Ptr UVLoop -> UVRunMode -> IO CInt
uvRun loop mode = throwUVIfMinus $ uv_run loop mode
foreign import ccall unsafe uv_run            :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
uvRunSafe :: HasCallStack => Ptr UVLoop -> UVRunMode -> IO CInt
uvRunSafe loop mode = throwUVIfMinus $ uv_run_safe loop mode
foreign import ccall safe "uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with usafe FFI.
uvAlive :: HasCallStack => Ptr UVLoop -> IO CInt
uvAlive loop = throwUVIfMinus $ uv_loop_alive loop
foreign import ccall unsafe uv_loop_alive :: Ptr UVLoop -> IO CInt

--------------------------------------------------------------------------------

data UVHandle
data UVContext

foreign import ccall unsafe hs_uv_handle_alloc  :: UVHandleType -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_close :: Ptr UVHandle -> IO ()

peekUVHandleContext :: Ptr UVHandle -> IO (Ptr UVContext)
peekUVHandleContext p =  #{peek uv_handle_t, data} p

pokeUVHandleContextMVar :: StablePtr PrimMVar -> Int -> Ptr UVContext -> IO () 
pokeUVHandleContextMVar mvar cap p = do
    #{poke hs_context_data, mvar}       p mvar
    #{poke hs_context_data, cap}        p (fromIntegral cap :: CInt)

pokeUVHandleContextBuffer :: Ptr Word8 -> Int -> Ptr UVContext -> IO () 
pokeUVHandleContextBuffer buf bufLen p = do
    #{poke hs_context_data, buffer}     p buf
    #{poke hs_context_data, buffer_siz} p (fromIntegral bufLen :: CSsize)

peekUVHandleContextResult :: Ptr UVContext -> IO Int
peekUVHandleContextResult p = fromIntegral <$> (#{peek hs_context_data, buffer_siz} p :: IO CSsize)

newtype UVHandleType = UVHandleType CInt 
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVHandleType, UVHandleType,
    uV_UNKNOWN_HANDLE  = UV_UNKNOWN_HANDLE,
    uV_ASYNC           = UV_ASYNC,
    uV_CHECK           = UV_CHECK,
    uV_FS_EVENT        = UV_FS_EVENT,
    uV_FS_POLL         = UV_FS_POLL,
    uV_HANDLE          = UV_HANDLE,
    uV_IDLE            = UV_IDLE,
    uV_NAMED_PIPE      = UV_NAMED_PIPE,
    uV_POLL            = UV_POLL,
    uV_PREPARE         = UV_PREPARE,
    uV_PROCESS         = UV_PROCESS,
    uV_STREAM          = UV_STREAM,
    uV_TCP             = UV_TCP,
    uV_TIMER           = UV_TIMER,
    uV_TTY             = UV_TTY,
    uV_UDP             = UV_UDP,
    uV_SIGNAL          = UV_SIGNAL,
    uV_FILE            = UV_FILE,
    uV_HANDLE_TYPE_MAX = UV_HANDLE_TYPE_MAX }

--------------------------------------------------------------------------------
-- uv_req_t

data UVReq

peekUVReqContext :: Ptr UVReq -> IO (Ptr UVContext)
peekUVReqContext p =  #{peek uv_req_t, data} p

initUVReq :: HasCallStack => UVReqType -> Resource (Ptr UVReq, Ptr UVContext)
initUVReq typ = initResource 
    (do req <- throwOOMIfNull (hs_uv_req_alloc typ)
        context <- peekUVReqContext req
        return (req, context)
    )
    (\ (req, _) -> hs_uv_req_free req)

pokeUVReqContextMVar :: StablePtr PrimMVar -> Int -> Ptr UVContext -> IO () 
pokeUVReqContextMVar mvar cap p = do
    #{poke hs_context_data, mvar}       p mvar
    #{poke hs_context_data, cap}        p (fromIntegral cap :: CInt)

pokeUVReqContextBuffer :: Ptr Word8 -> Int -> Ptr UVContext -> IO () 
pokeUVReqContextBuffer buf bufLen p = do
    #{poke hs_context_data, buffer}     p buf
    #{poke hs_context_data, buffer_siz} p (fromIntegral bufLen :: CSsize)

peekUVReqContextResult :: Ptr UVContext -> IO Int
peekUVReqContextResult p = fromIntegral <$> (#{peek hs_context_data, buffer_siz} p :: IO CSsize)

foreign import ccall unsafe hs_uv_req_alloc :: UVReqType -> IO (Ptr UVReq)
foreign import ccall unsafe hs_uv_req_free :: Ptr UVReq -> IO ()

newtype UVReqType = UVReqType CInt
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

#{enum UVReqType, UVReqType,
    uV_UNKNOWN_REQ      = UV_UNKNOWN_REQ,
    uV_REQ              = UV_REQ,
    uV_CONNECT          = UV_CONNECT,
    uV_WRITE            = UV_WRITE,
    uV_SHUTDOWN         = UV_SHUTDOWN,
    uV_UDP_SEND         = UV_UDP_SEND,
    uV_FS               = UV_FS,
    uV_WORK             = UV_WORK,
    uV_GETADDRINFO      = UV_GETADDRINFO,
    uV_GETNAMEINFO      = UV_GETNAMEINFO,
    uV_REQ_TYPE_MAX     = UV_REQ_TYPE_MAX }

--------------------------------------------------------------------------------

initUVAsyncWake :: HasCallStack => Ptr UVLoop -> Resource (Ptr UVHandle)
initUVAsyncWake loop = initResource 
    (do handle <- throwOOMIfNull (hs_uv_handle_alloc uV_ASYNC)
        throwUVIfMinus_ (hs_uv_async_wake_init loop handle) `onException` (hs_uv_handle_close handle)
        return handle
    )
    (hs_uv_handle_close) -- handle is free in uv_close callback

foreign import ccall unsafe hs_uv_async_wake_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

uvAsyncSend :: HasCallStack => Ptr UVHandle -> IO ()
uvAsyncSend = throwUVIfMinus_ . uv_async_send
foreign import ccall unsafe uv_async_send :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------

initUVTimer :: HasCallStack => Ptr UVLoop -> Resource (Ptr UVHandle)
initUVTimer loop = initResource 
    (do handle <- throwOOMIfNull (hs_uv_handle_alloc uV_TIMER)
        throwUVIfMinus_ (uv_timer_init loop handle) `onException` (hs_uv_handle_close handle)
        return handle
    )
    (hs_uv_handle_close) -- handle is free in uv_close callback
foreign import ccall unsafe uv_timer_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

uvTimerWakeStart :: HasCallStack => Ptr UVHandle -> Word64 -> IO ()
uvTimerWakeStart handle timeo = throwUVIfMinus_ $ hs_uv_timer_wake_start handle timeo
foreign import ccall unsafe hs_uv_timer_wake_start :: Ptr UVHandle -> Word64 -> IO CInt

uvTimerStart :: HasCallStack => Ptr UVHandle -> Word64 -> Word64 -> IO ()
uvTimerStart handle timeo repeat = throwUVIfMinus_ $ uv_timer_start handle timeo repeat
foreign import ccall unsafe uv_timer_start :: Ptr UVHandle -> Word64 -> Word64 -> IO CInt

uvTimerAgain :: HasCallStack => Ptr UVHandle -> IO ()
uvTimerAgain = throwUVIfMinus_ . uv_timer_again
foreign import ccall unsafe uv_timer_again :: Ptr UVHandle -> IO CInt

uvTimerSetRepeat :: HasCallStack => Ptr UVHandle -> Word64 -> IO ()
uvTimerSetRepeat handle timeo = throwUVIfMinus_ $ uv_timer_set_repeat handle timeo
foreign import ccall unsafe  uv_timer_set_repeat :: Ptr UVHandle -> Word64 -> IO CInt 

uvTimerStop :: HasCallStack => Ptr UVHandle -> IO ()
uvTimerStop = throwUVIfMinus_ . uv_timer_stop
foreign import ccall unsafe uv_timer_stop :: Ptr UVHandle -> IO CInt

--------------------------------------------------------------------------------

uvTCPOpen :: HasCallStack => Ptr UVHandle -> CInt -> IO ()
uvTCPOpen handle sock = throwUVIfMinus_ $ hs_uv_tcp_open handle sock
foreign import ccall unsafe hs_uv_tcp_open :: Ptr UVHandle -> CInt -> IO CInt

foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt

foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt

uvTCPNodelay :: HasCallStack => Ptr UVHandle -> Bool -> IO ()
uvTCPNodelay handle nodelay = throwUVIfMinus_ $ uv_tcp_nodelay handle (if nodelay then 1 else 0)
foreign import ccall unsafe uv_tcp_nodelay :: Ptr UVHandle -> CInt -> IO CInt

uvTCPKeepAlive :: HasCallStack => Ptr UVHandle -> Maybe CUInt -> IO ()
uvTCPKeepAlive handle Nothing = throwUVIfMinus_ $ uv_tcp_keepalive handle 0 0
uvTCPKeepAlive handle (Just delay) = throwUVIfMinus_ $ uv_tcp_keepalive handle 1 delay
foreign import ccall unsafe uv_tcp_keepalive :: Ptr UVHandle -> CInt -> CUInt -> IO CInt

uvTCPBind :: HasCallStack => Ptr UVHandle -> Ptr SockAddr -> Bool -> IO ()
uvTCPBind handle addr ipv6_only = throwUVIfMinus_ $
    uv_tcp_bind handle addr (if ipv6_only then #{const UV_TCP_IPV6ONLY} else 0)
foreign import ccall unsafe uv_tcp_bind :: Ptr UVHandle -> Ptr SockAddr -> CUInt -> IO CInt

uvTCPConnect :: HasCallStack => Ptr UVReq -> Ptr UVHandle -> Ptr SockAddr -> IO ()
uvTCPConnect req handle addr = throwUVIfMinus_ $ hs_uv_tcp_connect req handle addr
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVReq -> Ptr UVHandle -> Ptr SockAddr -> IO CInt

--------------------------------------------------------------------------------

