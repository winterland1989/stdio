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
    withForeignPtr tty $ \ p ->
        uv_tty_init loop p fd (if readable then 1 else 0)

    return (TTY tty loop slot uvm)

instance Input TTY where
    inputInfo _ = "tty"
    readInput (TTY tty loop slot uvm) buf bufSiz = withMVar (uvmFreeSlotList uvm) $ \ _ -> do
        btable <- readIORef $ uvmBlockTable uvm
        print (uvmLoopData uvm)
        (bufTable, bufSizTable) <- peekReadBuffer (uvmLoopData uvm)
        print bufTable
        print "1"
        pokeElemOff bufTable slot buf
        print "2"
        pokeElemOff bufSizTable slot (fromIntegral bufSiz)
        print "3"
        takeMVar (indexArr btable slot)
        print "4"
        rTable <- peekResultTable (uvmLoopData uvm)
        print "5"
        fromIntegral `fmap` peekElemOff rTable slot




