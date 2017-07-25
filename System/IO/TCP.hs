module System.IO.TCP where

import System.IO.UV.FFI
import System.IO.UV.Manager
import System.IO.UV.Exception
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
    , tcpSlot  :: Int
    , tcpManager :: UVManager
    , tcpReadLock :: MVar ()
    }

newTCP :: CInt -> IO TCP
newTCP fd = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
    p <- hs_handle_init uV_TCP
    readLock <- newMVar ()

    uv_tcp_init loop p
    uv_tcp_open p fd
    uv_stream_set_blocking p 0
    poke_uv_handle_data p (fromIntegral slot)

    return (TCP p slot uvm readLock)

closeTCP :: TCP -> IO ()
closeTCP = undefined

instance Input TCP where
    inputInfo _ = "tcp"
    readInput (TCP tcp slot uvm readLock) buf bufSiz = withMVar readLock $ \ _ -> do
        ensureUVMangerRunning uvm
        (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            resultTable <- peekResultTable (uvmLoopData uvm)
            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            pokeElemOff resultTable slot 0
            hs_read_start tcp
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        fromIntegral `fmap` peekElemOff bufSizTable slot
