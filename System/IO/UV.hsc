module System.IO.UV where

import Foreign
import Foreign.C
import Data.Primitive.Addr

#include <uv.h>

data UVLoopData = UVLoopData 
    { uvLoopBufferTable :: Ptr Addr
    , uVLoopEventCounter :: Ptr Int
    , uvLoopEventQueue :: Ptr Int
    }

instance Storable UVLoopData where
    sizeOf _ = sizeOf (undefined :: Word) * 2
    alignment _ = alignment (undefined :: Word)
    poke p (UVLoopData p1 p2 p3) = do
        let pp = castPtr p
        poke pp p1
        poke (pp `plusPtr` 1) p2
        poke (pp `plusPtr` 2) p3

    peek p = 
        let pp = castPtr p
        in UVLoopData 
            <$> peek pp 
            <*> peek (pp `plusPtr` 1)
            <*> peek (pp `plusPtr` 2)

data UVLoop = UVLoop { uvLoopData :: Ptr UVLoopData }

instance Storable UVLoop where
    sizeOf _ = #size uv_loop_t
    alignment _ = alignment (undefined :: Word)
    poke p uvloop = do
        #{poke uv_loop_t, data} p $ uvLoopData uvloop
    peek p = UVLoop 
        <$> (#{peek uv_loop_t, data} p)

#{enum CInt, , UV_RUN_DEFAULT, UV_RUN_ONCE, UV_RUN_NOWAIT}

foreign import ccall unsafe "uv_loop_init" uvLoopInit :: Ptr UVLoop -> IO ()

data UVHandle = UVHandle 
    { uvHandleData :: Int 
    , uvHandleType :: CInt
    , uvHandleLoop :: Ptr UVLoop
    }

instance Storable UVHandle where
    sizeOf _ = #size uv_handle_t
    alignment _ = alignment (undefined :: Word)
    poke p uvhandle = do
        #{poke uv_handle_t, data} p $ uvHandleData uvhandle
        #{poke uv_handle_t, type} p $ uvHandleType uvhandle
        #{poke uv_handle_t, loop} p $ uvHandleLoop uvhandle
    peek p = UVHandle 
        <$> (#{peek uv_handle_t, data} p)
        <*> (#{peek uv_handle_t, type} p)
        <*> (#{peek uv_handle_t, loop} p)

#{enum CInt, , UV_UNKNOWN_HANDLE, UV_ASYNC, UV_CHECK, UV_FS_EVENT, UV_FS_POLL, UV_HANDLE, UV_IDLE, UV_NAMED_PIPE, UV_POLL, UV_PREPARE, UV_PROCESS, UV_STREAM, UV_TCP, UV_TIMER, UV_TTY, UV_UDP, UV_SIGNAL, UV_FILE, UV_HANDLE_TYPE_MAX }


data UVFs = UVFs
    { uvFsData :: CInt
    , uvFsLoop :: Ptr UVLoop
    , uvFsType :: CInt
    , uvFsPath :: CString   -- TODO: windows need to use CWString
    , uvFsResult :: CInt
    , uvFsStat :: UVStat
    }

instance Storable UVReq where
    sizeOf _ = #size uv_handle_t
    alignment _ = alignment (undefined :: Word)
    poke p uvhandle = do
        #{poke uv_handle_t, data} p $ uvReqData uvhandle
        #{poke uv_handle_t, type} p $ uvReqType uvhandle
        #{poke uv_handle_t, loop} p $ uvReqLoop uvhandle
    peek p = UVReq 
        <$> (#{peek uv_handle_t, data} p)
        <*> (#{peek uv_handle_t, type} p)
        <*> (#{peek uv_handle_t, loop} p)

#{enum CInt, , UV_UNKNOWN_HANDLE, UV_ASYNC, UV_CHECK, UV_FS_EVENT, UV_FS_POLL, UV_HANDLE, UV_IDLE, UV_NAMED_PIPE, UV_POLL, UV_PREPARE, UV_PROCESS, UV_STREAM, UV_TCP, UV_TIMER, UV_TTY, UV_UDP, UV_SIGNAL, UV_FILE, UV_HANDLE_TYPE_MAX }
