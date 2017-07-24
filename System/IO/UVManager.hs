{-|
Module      : System.IO.UVManager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

-}

module System.IO.UVManager where

import System.IO.UV
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Foreign
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Primitive.Addr
import System.IO.Unsafe

initTableSize :: Int
initTableSize = 128

data UVManager = UVManager
    { uvmBlockTable  :: Array (MVar ())             -- a list to store blocked thread

    , uvmFreeSlotList :: MVar [Int]                 -- we generate an unique range limited 'Int' /slot/
                                                    -- for each uv_handle_t and uv_req_t

                                                    -- the slot is attached as the data field of
                                                    -- c struct such as handle and request,
                                                    -- thus it will be available to c callback
                                                    -- inside callback we increase event counter and
                                                    -- push slot into event queue
                                                    --
                                                    -- after uv_run is finished, we read the counter
                                                    -- and the queue back, use the slots in the queue
                                                    -- as index to unblock thread in block queue
                                                    --
                                                    -- We also use this 'MVar' to do finalization of
                                                    -- I/O manager by attaching finalizers to it with 'mkWeakMVar'

    , uvmBufferTable :: Ptr (Ptr Word8)             -- a list to keep buffer's refrerence
    , uvmBufferSizeTable :: Ptr CSize               -- a list to keep buffer's size
                                                    -- freed when free slot list run out of scope
                                                    --
    , uvmResultTable :: Ptr CSize                   -- a list to keep callback return value
                                                    -- such as file or read bytes number
                                                    -- freed when free slot list run out of scope

    , uvmEventCounter :: Ptr CSize                  -- These two fields compose a special data structure
    , uvmEventQueue   :: Ptr CSize                  -- to keep trace of events during a uv_run
                                                    -- they are attach to uv_loop_t struct as
                                                    -- data field together with buffer and result table
                                                    -- before each uv_run they are cleared
                                                    --
                                                    -- freed when free slot list run out of scope

    , uvmLoopData    :: Ptr UVLoopData              -- This is the pointer we pass to uv_loop_t as
                                                    -- data field, it contains above fields
                                                    --
                                                    -- freed when free slot list run out of scope

    , uvmLoop        :: Ptr UVLoop                  -- the uv loop refrerence
                                                    --
                                                    -- close and freed when free slot list run out of scope
    }

uvManager :: IORef (Array UVManager)
uvManager = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    forM [0..numCaps-1] $ \ i -> do
        writeArr uvmArray i =<< newUVManager initTableSize
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

getUVManager :: IO UVManager
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManager
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

newUVManager :: Int -> IO UVManager
newUVManager siz = do

    blockTable <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr blockTable i =<< newEmptyMVar
    iBlockTable <- unsafeFreezeArr blockTable

    freeSlotList <- newMVar [0..siz-1]

    bufferTable <- mallocBytes siz'
    bufferSize <- mallocBytes siz''
    resultTable <- mallocBytes siz''

    eventCounter <- malloc
    eventQueue <- mallocBytes siz''

    uvLoop <- malloc
    uvLoopData <- malloc

    poke uvLoopData (UVLoopData bufferTable bufferSize resultTable eventCounter eventQueue)
    poke uvLoop (UVLoop uvLoopData)

    uvLoopInit uvLoop

    _ <- mkWeakMVar freeSlotList $ do
        free bufferTable
        free resultTable
        free eventCounter
        free eventQueue
        free uvLoopData
        uvLoopClose uvLoop
        free uvLoop

    return (UVManager iBlockTable freeSlotList bufferTable bufferSize resultTable eventCounter eventQueue uvLoopData uvLoop)

  where
    siz' = siz * sizeOf (undefined :: Ptr ())
    siz'' = siz * sizeOf (undefined :: CSize)
