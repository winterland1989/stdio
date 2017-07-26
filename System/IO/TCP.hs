module System.IO.TCP where

import System.IO.UV
import System.IO.UVManager
import System.IO.Handle
import Foreign
import Foreign.C
import GHC.Stack
import Control.Monad
import Data.IORef
import Control.Concurrent.MVar
import Data.Array


data TCP = TCP
    { tcpHandle :: Ptr UVHandle
    , tcpLoop  :: Ptr UVLoop
    , tcpSlot  :: Int
    , tcpManager :: UVManager
    , tcpReadLock :: MVar ()
    }

newTCP :: CInt -> IO TCP
newTCP fd = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
    p <- mallocBytes . fromIntegral  =<< uv_handle_size (getUVHandleType uV_TCP)
    readLock <- newMVar ()

    uv_tcp_init loop p
    uv_tcp_open p fd
    uv_stream_set_blocking p 0
    poke_uv_handle_data p (fromIntegral slot)

    return (TCP p loop slot uvm readLock)

instance Input TCP where
    inputInfo _ = "tcp"
    readInput (TCP tcp loop slot uvm readLock) buf bufSiz = withMVar readLock $ \ _ -> do
        ensureUVMangerRunning uvm
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
            resultTable <- peekResultTable (uvmLoopData uvm)
            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            pokeElemOff resultTable slot 0
            hs_read_start tcp
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        rTable <- peekResultTable (uvmLoopData uvm)
        fromIntegral `fmap` peekElemOff rTable slot
