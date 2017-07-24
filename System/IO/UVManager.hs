module System.IO.UVManager where

import System.IO.UV
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Foreign
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Primitive.Addr

initQueueSize :: Int
initQueueSize = 128

data UVManager = UVManager
    { umBlockTable  :: Array (MVar ())              -- a list to store blocked thread
    , umBufferTable :: MutablePrimArray RealWorld Addr  -- a list to keep using buffer's refrerence

    , umEventCounter :: Ptr Int
    , umEventQueue  :: MutablePrimArray RealWorld Int   -- This is a special data structure

    , umFreeSlotList :: MVar [Int]                  -- we generate an unique range limited 'Int' /slot/
                                                    -- for each uv_handle_t and uv_req_t
                                                    -- the slot is attached as the data field of
                                                    -- c struct, and will be available to c callback
                                                    -- then we use the slot as index to unblock
                                                    -- thread in uvBlockQueue
                                                    --
    , umLoop        :: Ptr UVLoop                   -- the uv loop refrerence
                                                    -- need manually free
    }

startUVLoop :: Int -> IO UVManager
startUVLoop siz = do

    blockTable <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTable i =<< newEmptyMVar
    iBlockTable <- unsafeFreezeArr blockTable

    bufferTable <- newPinnedPrimArray siz
    setArr bufferTable 0 siz nullAddr

    eventCounter <- malloc
    eventQueue <- newPinnedPrimArray siz

    freeSlotList <- newMVar [0..siz-1]

    uvLoop <- malloc
    uvLoopData <- malloc

    poke uvLoopData
        (UVLoopData
            (mutablePrimArrayContents bufferTable)
            (mutablePrimArrayContents eventQueue)
            eventCounter)
    poke uvLoop (UVLoop uvLoopData)

    uvLoopInit uvLoop
    return (UVManager
        iBlockTable bufferTable eventCounter eventQueue freeSlotList uvLoop)
