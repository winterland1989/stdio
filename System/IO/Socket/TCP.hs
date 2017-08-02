module System.IO.TCP where

import System.IO.UV.Base
import System.IO.UV.Manager
import qualified System.IO.UV.Exception as E
import qualified System.IO.Exception as E
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

newTCP :: HasCallStack => CInt -> IO TCP
newTCP fd = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
        dev = "tcp FD:" ++ show fd
    p <- E.throwOOMIfNull callStack dev $ hs_handle_init uV_TCP
    readLock <- newMVar ()

    withMVar (uvmFreeSlotList uvm) $ \ _ -> do
        E.throwUVErrno callStack dev $ uv_tcp_init loop p

    E.throwUVErrno callStack dev $ uv_tcp_open p fd

    poke_uv_handle_data p (fromIntegral slot)
    return (TCP p slot uvm readLock)

closeTCP :: TCP -> IO ()
closeTCP = undefined

instance Input TCP where
    inputInfo tcp = "tcp" -- TODO: make it detailed
    readInput tcp@(TCP uvhandle slot uvm readLock) buf bufSiz = withMVar readLock $ \ _ -> do
        ensureUVMangerRunning uvm
        (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
        resultTable <- peekResultTable (uvmLoopData uvm)
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do

            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            pokeElemOff resultTable slot 0
            E.throwUVErrno callStack  (inputInfo tcp) $ hs_read_start uvhandle
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        fromIntegral `fmap` peekElemOff resultTable slot
