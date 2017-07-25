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
    { ttyPtr  :: ForeignPtr UVTTY
    , ttyLoop :: Ptr UVLoop
    , ttySlot :: Int
    , ttyUVManager :: UVManager
    }

newTTY :: CInt -> Bool -> IO TTY
newTTY fd readable = do
    uvm <- getUVManager
    slot <- allocSlot uvm
    let loop = (uvmLoop uvm)
    tty <- mallocForeignPtr
    withForeignPtr tty $ \ p -> do
        uv_tty_init loop p fd (if readable then 1 else 0)
        poke_uv_tty_data p slot

    return (TTY tty loop slot uvm)

instance Input TTY where
    inputInfo _ = "tty"
    readInput (TTY tty loop slot uvm) buf bufSiz = do
        withMVar (uvmFreeSlotList uvm) $ \ _ -> do
            (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
            print "1"
            pokeElemOff bufTable slot buf
            print "2"
            pokeElemOff bufSizTable slot (fromIntegral bufSiz)
            print "3"
            withForeignPtr tty $ \ p -> hs_read_start p
        print "4"
        btable <- readIORef $ uvmBlockTable uvm
        takeMVar (indexArr btable slot)
        print "5"
        rTable <- peekResultTable (uvmLoopData uvm)
        print "6"
        fromIntegral `fmap` peekElemOff rTable slot




