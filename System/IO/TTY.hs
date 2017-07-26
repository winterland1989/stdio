module System.IO.TTY where

import System.IO.UV
import System.IO.Handle
import System.IO.UVManager
import Foreign
import Foreign.C
import GHC.Stack
import Data.IORef
import Control.Concurrent.MVar
import Data.Array

data TTY = TTY
    { ttyPtr  :: ForeignPtr UVHandle
    , ttyLoop :: Ptr UVLoop
    , ttySlot :: Int
    , ttyUVManager :: UVManager
    }

newTTY :: CInt -> Bool -> IO TTY
newTTY fd readable = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
    tty <- mallocUVHandle uV_TTY
    withForeignPtr tty $ \ p -> do
        uv_tty_init loop p fd (if readable then 1 else 0)
        poke_uv_handle_data p slot
    return (TTY tty loop slot uvm)

instance Input TTY where
    inputInfo _ = "tty"
    readInput (TTY tty loop slot uvm) buf bufSiz = do
        ensureUVMangerRunning uvm
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
            pokeElemOff bufTable slot buf
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            withForeignPtr tty $ \ p -> hs_read_start p
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        rTable <- peekResultTable (uvmLoopData uvm)
        fromIntegral `fmap` peekElemOff rTable slot
