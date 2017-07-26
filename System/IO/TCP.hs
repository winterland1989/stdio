module System.IO.TCP where

import System.IO.UV
import System.IO.UVManager
import System.IO.Handle
import Foreign
import Foreign.C
import GHC.Stack
import Data.IORef
import Control.Concurrent.MVar
import Data.Array


data TCP = TCP
    { tcpHandle :: ForeignPtr UVHandle
    , tcpLoop  :: Ptr UVLoop
    , tcpSlot  :: Int
    , tcpManager :: UVManager
    }

newTCP :: CInt -> IO TCP
newTCP fd = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
    tcp <- mallocUVHandle uV_TCP
    withForeignPtr tcp $ \ p -> do
        uv_tcp_init loop p
        uv_tcp_open p fd
        uv_stream_set_blocking p 0
        poke_uv_handle_data p slot
    return (TCP tcp loop slot uvm)

instance Input TCP where
    inputInfo _ = "tcp"
    readInput (TCP tcp loop slot uvm) buf bufSiz = do
        ensureUVMangerRunning uvm
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            withForeignPtr tcp $ \ p -> hs_read_start p
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        rTable <- peekResultTable (uvmLoopData uvm)
        fromIntegral `fmap` peekElemOff rTable slot
