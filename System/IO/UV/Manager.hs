{-# LANGUAGE BangPatterns #-}

{-|
Module      : System.IO.UV.Manager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

-}

module System.IO.UV.Manager
  ( UVManager
  , getUVManager
  , getBlockMVar
  , pokeBufferTable
  , getResult
  , allocSlot
  , freeSlot
  , withUVManager
  , withUVManagerEnsureRunning
  ) where

import GHC.Stack.Compat
import qualified System.IO.Exception as E
import qualified System.IO.UV.Exception as E
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
import Foreign hiding (void)
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Primitive.Addr
import System.IO.Unsafe
import System.IO.UV.Base
import System.Posix.Types (CSsize(..))

--------------------------------------------------------------------------------

data UVRunningState
    = UVRunning
    | UVBlocking
    | UVStopped
  deriving (Show, Eq, Ord)

data UVManager = UVManager
    { uvmBlockTable  :: IORef (UnliftedArray (MVar ()))     -- a array to store thread blocked on read or write

    , uvmFreeSlotList :: MVar [Int]                 -- we generate two unique range limited 'Int' /slot/
                                                    -- for each uv_handle_t(one for read and another for
                                                    -- write).
                                                    --
                                                    -- the slot is attached as the data field of
                                                    -- c struct uv_handle_t, thus will be available
                                                    -- to c callbacks, which are static functions:
                                                    -- inside callback we increase event counter and
                                                    -- push slot into event queue
                                                    --
                                                    -- after uv_run is finished, we read the counter
                                                    -- and the queue back, use the slots in the queue
                                                    -- as index to unblock thread in block queue
                                                    --
                                                    -- We also use this 'MVar' to do finalization of
                                                    -- I/O manager by attaching finalizers to it with 'mkWeakMVar'

    , uvmLoop        :: Ptr UVLoop                  -- the uv loop refrerence
    , uvmLoopData    :: Ptr UVLoopData              -- This is the pointer to uv_loop_t's data field

    , uvmRunningLock :: MVar UVRunningState         -- during uv_run this lock shall be held!
                                                    -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                    -- thus we can only add new event when uv_run is not
                                                    -- running, usually this is not a problem because
                                                    -- unsafe FFI can't run concurrently on one
                                                    -- capability, but with work stealing we'd better
                                                    -- ask for this lock before calling any uv APIs.

    , uvmAsync       :: Ptr UVHandle                -- This ayncs handle is used when we want to break from
                                                    -- a blocking uv_run, it will call a prdefined callback
                                                    -- to stop the loop. This is the only thread safe
                                                    -- wake up mechanism for libuv.

    , uvmIdleCounter :: Counter                     -- Counter for idle(no event) uv_runs, when counter
                                                    -- reaches 10, we start to increase waiting between
                                                    -- uv_run, until delay reach 8 milliseconds

    , uvmCap :: Int                                 -- the capability uv manager should run on, we save this
                                                    -- number for restarting uv manager
    }

initTableSize :: Int
initTableSize = 128

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    forM [0..numCaps-1] $ \ i -> do
        writeArr uvmArray i =<< newUVManager initTableSize i
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

getUVManager :: IO UVManager
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManagerArray
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

getBlockMVar :: UVManager -> Int -> IO (MVar ())
getBlockMVar uvm slot = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable slot

pokeBufferTable :: UVManager -> Int -> Ptr Word8 -> Int -> IO ()
pokeBufferTable uvm slot buf bufSiz = withMVar (uvmFreeSlotList uvm) $  \ _ -> do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    pokeElemOff bufTable slot buf
    pokeElemOff bufSizTable slot (fromIntegral bufSiz)

getResult :: UVManager -> Int -> IO (E.UVReturn CSsize)
getResult uvm slot = withMVar (uvmFreeSlotList uvm) $  \ _ -> do
    resultTable <- peekUVResultTable (uvmLoopData uvm)
    r <- peekElemOff resultTable slot
    return (E.UVReturn r)

newUVManager :: HasCallStack => Int -> Int -> IO UVManager
newUVManager siz cap = do
    mblockTable <- newArr siz
    forM_ [0..siz-1] $ \ i ->
        writeArr mblockTable i =<< newEmptyMVar
    blockTable <- unsafeFreezeArr mblockTable
    blockTableRef <- newIORef blockTable

    freeSlotList <- newMVar [0..siz-1]

    loop <- E.throwOOMIfNull "uv manager" $
        hs_loop_init (fromIntegral siz)

    loopData <- peek_uv_loop_data loop

    runningLock <- newMVar UVStopped

    async <- E.throwOOMIfNull "malloc block async handler for uv manager" $ hs_handle_init uV_ASYNC
    hs_async_init_stop_loop loop async

    idleCounter <- newCounter 0

    _ <- mkWeakIORef blockTableRef $ do hs_loop_close loop

    return (UVManager blockTableRef freeSlotList loop loopData runningLock async idleCounter cap)

-- | libuv is not thread safe, use this function to perform handle/request initialization.
--
withUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
withUVManager uvm f = do
    r <- modifyMVar (uvmRunningLock uvm) $ \ running ->
        case running of
            UVBlocking -> do
                E.throwIfError "uvm async handle" $ uv_async_send (uvmAsync uvm)
                return (UVBlocking, Nothing)
            s -> do
                r <- f (uvmLoop uvm)
                return (s, Just r)

    case r of
        Just r' -> return r'
        _       -> yield >> withUVManager uvm f

-- | libuv is not thread safe, use this function to start reading/writing.
--
-- This function also take care of restart uv manager in case of stopped.
--
withUVManagerEnsureRunning :: HasCallStack => UVManager -> IO a -> IO a
withUVManagerEnsureRunning uvm f = do
    r <- modifyMVar (uvmRunningLock uvm) $ \ running ->
        case running of
            UVStopped -> do
                r <- f
                void $ forkOn (uvmCap uvm) (startUVManager uvm)
                return (UVRunning, Just r)
            UVBlocking -> do
                E.throwIfError "uvm async handle" $ uv_async_send (uvmAsync uvm)
                return (UVBlocking, Nothing)
            s -> do
                r <- f
                return (s, Just r)

    case r of
        Just r' -> return r'
        _       -> yield >> withUVManagerEnsureRunning uvm f


-- | Start the uv loop, the loop is stopped when there're not active I/O requests.
--
-- Inside loop, we do either blocking wait or non-blocking wait depending on idle counter.
--
startUVManager :: UVManager -> IO ()
startUVManager uvm = do
    continue <- modifyMVar (uvmRunningLock uvm) $ \ _ -> do
        c <- uv_loop_alive(uvmLoop uvm)     -- we're holding the uv lock so no more new request can be add here
        if (c /= 0)
        then do
            let idleCounter = uvmIdleCounter uvm
            e <- step uvm False
            ic <- readIORefU idleCounter
            if (e == 0)                     -- bump the idle counter if no events, there's no need to do atomic-ops
            then when (ic < 50) $ writeIORefU idleCounter (ic+1)
            else writeIORefU idleCounter 0

            return (UVRunning, True)
        else return (UVStopped, False)

    -- If not continue, new events will find running is locking on 'False'
    -- and fork new uv manager thread.
    when continue $ do
        let idleCounter = uvmIdleCounter uvm
        ic <- readIORefU idleCounter
        if (ic >= 50)                   -- we yield 50 times, then start a blocking wait if still no events coming
        then do
            _ <- swapMVar (uvmRunningLock uvm) UVBlocking   -- after changing this, other thread can wake up us
            step uvm True                                   -- by send async handler, and it's thread safe
            _ <- swapMVar (uvmRunningLock uvm) UVRunning    -- it's OK to send multiple time, libuv only calls
            writeIORefU idleCounter 0                       -- async callback once.
            yield       -- we yield here, to give other thread a chance to register new event
        else yield      -- it's important that we yeild enough time, CPU vs performance trade off here
        startUVManager uvm

  where
    -- call uv_run, return the event number
    --
    step :: UVManager -> Bool -> IO CSize
    step (UVManager blockTableRef freeSlotList loop loopData _ _ _ _) block = do
            blockTable <- readIORef blockTableRef

            clearUVEventCounter loopData

            E.retryInterrupt "uv manager uv_run" $
                if block
                then uv_run_safe loop uV_RUN_ONCE
                else uv_run loop uV_RUN_NOWAIT

            (c, q) <- peekUVEventQueue loopData
            forM_ [0..(fromIntegral c-1)] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                void $ tryPutMVar lock ()
            return c

allocSlot :: UVManager -> IO Int
allocSlot uvm@(UVManager blockTableRef freeSlotList loop _ _ _ _ _) = withUVManager uvm $ \ _ -> do
    modifyMVar freeSlotList $ \ freeList -> case freeList of
        (s:ss) -> return (ss, s)
        []     -> do
            -- free list is empty, we double it
            -- but we shouldn't do it if uv_run doesn't finish yet
            -- because we may re-allocate loop data in another thread

            blockTable <- readIORef blockTableRef
            let oldSiz = sizeofArr blockTable
                newSiz = oldSiz * 2

            blockTable' <- newArr newSiz
            copyArr blockTable' 0 blockTable 0 oldSiz

            forM_ [oldSiz..newSiz-1] $ \ i ->
                writeArr blockTable' i =<< newEmptyMVar
            !iBlockTable' <- unsafeFreezeArr blockTable'

            writeIORef blockTableRef iBlockTable'

            hs_loop_resize loop (fromIntegral newSiz)

            return ([oldSiz+1..newSiz-1], oldSiz)    -- fill the free slot list

freeSlot :: Int -> UVManager -> IO ()
freeSlot slot  (UVManager _ freeSlotList _ _ _ _ _ _) =
    modifyMVar_ freeSlotList $ \ freeList -> return (slot:freeList)
