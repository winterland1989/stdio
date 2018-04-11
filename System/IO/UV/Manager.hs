{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : System.IO.UV.Manager
Description : I/O manager based on libuv
Copyright   : (c) Winterland, 2017
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provide I/O manager which bridge libuv's async interface with ghc's light weight thread.

The main procedures for doing event I/O is:

+ Allocate a slot number using 'allocSlot'.
+ Prepare you I/O buffer and write them to uv loop with 'pokeBufferTable'(both read and write).
+ Block your thread with a 'MVar', using 'getBlockMVar' to get it.
+ Read the result with 'getResult', for read it's the read bytes number, for write it will be zero.
  Use 'E.throwIfError' to guard error situations.
+ Return the slot back uv manager with 'freeSlot'.

Usually slots are cache in the I/O device so that you don't have to allocate new one before each I/O operation.
Check "System.IO.Socket.TCP" as an example.

-}

module System.IO.UV.Manager
  ( UVManager
  , getUVManager
  , getBlockMVar
  , pokeBufferTable
  , withUVManager
  , withUVManager'
  , initUVSlot
  , initUVHandle
  , forkBa
  ) where

import GHC.Stack.Compat
import qualified System.IO.Exception as E
import qualified System.IO.UV.Exception as E
import Data.Array
import Data.Primitive.PrimArray
import Data.Word
import Data.IORef
import Data.IORef.Unboxed
import Foreign hiding (void, with)
import Foreign.C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.QSemN
import Control.Monad
import Control.Monad.IO.Class
import Data.Primitive.Addr
import System.IO.Unsafe
import System.Posix.Types (CSsize(..))
import System.IO.Exception
import System.IO.UV.Internal

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable   :: {-# UNPACK #-} !(IORef (UnliftedArray (MVar Int))) -- a array to store thread blocked on read or write
                                                    -- Int inside MVar is the async action's result

    , uvmFreeSlotList :: {-# UNPACK #-} !(MVar [UVSlot])              -- we generate two unique range limited 'Int' /slot/
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


    , uvmLoop        :: {-# UNPACK #-} !(Ptr UVLoop)                 -- the uv loop refrerence
    , uvmLoopData    :: {-# UNPACK #-} !(Ptr UVLoopData)            -- This is the pointer to uv_loop_t's data field

    , uvmRunning     :: {-# UNPACK #-} !(MVar Bool)                     -- only uv manager thread will modify this value
                                                    -- 'True' druing uv_run and 'False' otherwise.
                                                    --
                                                    -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                    -- thus we can only add new event when uv_run is not
                                                    -- running, usually this is not a problem because
                                                    -- unsafe FFI can't run concurrently on one
                                                    -- capability, but with work stealing/multi-threaded
                                                    -- uv_accept we have to ask for this lock before
                                                    -- calling any uv APIs.
                                                    --
    , uvmAsync       :: {-# UNPACK #-} !(Ptr UVHandle)       -- This async handle is used when we want to break from
                                                    -- a blocking uv_run, send async request is the only
                                                    -- thread safe wake up mechanism for libuv.



    , uvmTimer       :: {-# UNPACK #-} !(Ptr UVHandle) -- This timer handle is used when we want to break from
                                                    -- a blocking uv_run in non-threaded GHC rts.

    , uvmIdleCounter :: {-# UNPACK #-} !(Counter)      -- Counter for idle(no event) uv_runs, when counter
                                                    -- reaches 10, we start to increase waiting between
                                                    -- uv_run, until delay reach 8 milliseconds

    , uvmCap ::  {-# UNPACK #-} !Int                 -- the capability uv manager should run on, we save this
                                                    -- number for starting uv manager with `forkOn`
    }

instance Eq UVManager where
    uvm == uvm' =
        uvmCap uvm == uvmCap uvm'

initTableSize :: Int
initTableSize = 128

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    s <- newQSemN 0
    forM [0..numCaps-1] $ \ i -> do
        -- fork uv manager thread
        forkOn i . withResource (initUVManager initTableSize i) $ \ m -> do
            writeArr uvmArray i m
            signalQSemN s 1
            startUVManager m
    waitQSemN s numCaps
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

-- | Get 'UVManager' runing on the same capability.
--
getUVManager :: IO UVManager
{-# INLINABLE getUVManager #-}
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManagerArray
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

-- | Get 'MVar' from blocking table with given slot.
--
getBlockMVar :: UVManager -> UVSlot -> IO (MVar Int)
{-# INLINABLE getBlockMVar #-}
getBlockMVar uvm (UVSlot slot) = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable (fromIntegral slot)

-- | Poke a prepared buffer and size into loop data under given slot.
--
-- NOTE, this is not protected with 'withUVManager' for effcient reason, you should merge this action
-- with other uv action and put them together inside a 'withUVManager' or 'withUVManager\''. for example:
--
--  @@@
--      ...
--      withUVManager' uvm $ do
--          pokeBufferTable uvm rslot buf len
--          uvReadStart handle
--      ...
--  @@@
--
pokeBufferTable :: UVManager -> UVSlot -> Ptr Word8 -> Int -> IO ()
{-# INLINABLE pokeBufferTable #-}
pokeBufferTable uvm (UVSlot slot) buf bufSiz = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    pokeElemOff bufTable (fromIntegral slot) buf
    pokeElemOff bufSizTable (fromIntegral slot) (fromIntegral bufSiz)

initUVManager :: HasCallStack => Int -> Int -> Resource UVManager
initUVManager siz cap = do
    loop  <- initUVLoop (fromIntegral siz)
    async <- initUVAsyncWake loop
    timer <- initUVTimer loop

    liftIO $ do
        mblockTable <- newArr siz
        forM_ [0..siz-1] $ \ i ->
            writeArr mblockTable i =<< newEmptyMVar
        blockTable <- unsafeFreezeArr mblockTable
        blockTableRef <- newIORef blockTable

        freeSlotList <- newMVar [0..(fromIntegral siz)-1]

        loopData <- peekUVLoopData loop

        running <- newMVar False

        idleCounter <- newCounter 0

        return (UVManager blockTableRef freeSlotList loop loopData running async timer idleCounter cap)

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- libuv is not thread safe, use this function to perform any action which will mutate uv_loop's state.
--
withUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
withUVManager uvm f = do

    r <- withMVar (uvmRunning uvm) $ \ running ->
        if running
        then do
            uvAsyncSend (uvmAsync uvm) -- if uv_run is running, it will stop
                                       -- if uv_run is not running, next running won't block
            return Nothing
        else do
            r <- f (uvmLoop uvm)
            return (Just r)

    case r of
        Just r' -> return r'
        _       -> yield >> withUVManager uvm f -- we yield here, because uv_run is probably not finished yet

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- Some action did not request uv_loop pointer explicitly, but will mutate uv_loop underhood, for example:
-- @uv_read_start@. These actions have to be protected by locking the uv_loop.
--
-- In fact most of the libuv's functions are not thread safe, and 'uv_run' can be running concurrently
-- without this lock.
--
withUVManager' :: HasCallStack => UVManager -> IO a -> IO a
withUVManager' uvm f = withUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
-- Inside loop, we do either blocking wait or non-blocking wait depending on idle counter.
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ _ _ _ running _ _ idleCounter _) = do

    ic <- readIORefU idleCounter
    e <- if
        | ic < 100 -> do        -- we really don't want enter safe FFI too often
                                -- but if we keep doing short timeout poll
                                -- we're facing the danger killing idle GC
                                -- polling with 1ms timeout for 100 times seems reasonable
                                -- 0.1s is a balance between scheduling overhead,
                                -- and idle GC interval.
            e <- withMVar running $ \ _ -> step uvm False
            yield
            return e
        | otherwise -> do
            -- let's do a blocking poll
            _ <- swapMVar running True         -- after changing this, other thread can wake up us
            e <- step uvm True                 -- by send async handler, and it's thread safe
            _ <- swapMVar running False
            yield                              -- we yield here, to give other thread a chance to register new event
            return e

    if (e == 0)                            -- bump the idle counter if no events, there's no need to do atomic-ops
    then writeIORefU idleCounter (ic+1)
    else writeIORefU idleCounter 0

    startUVManager uvm

  where
    -- call uv_run, return the event number
    step :: UVManager -> Bool -> IO CSize
    step (UVManager blockTableRef freeSlotList loop loopData _ _ timer _ _) block = do
            blockTable <- readIORef blockTableRef
            clearUVEventCounter loopData        -- clean event counter

            if block
            then if rtsSupportsBoundThreads
                then do
                    uvTimerStop timer
                    -- if rts support multiple capability, we choose safe FFI call with run once mode
                    void $ uvRunSafe loop uV_RUN_ONCE
                else do
                    -- use a 8ms timeout blocking poll on non-threaded rts
                    uvTimerWakeStart timer 8
                    void $ uvRun loop uV_RUN_ONCE
            else do
                -- we use uV_RUN_ONCE with 1ms timeout instead of uV_RUN_NOWAIT
                -- because we don't want to spin CPU cycles on GHC scheduler too much
                -- and 1ms is the shortest timeout we can achieve with libuv's timer
                uvTimerWakeStart timer 1
                void $ uvRun loop uV_RUN_ONCE

            (c, q) <- peekUVEventQueue loopData
            resultTable <- peekUVResultTable (uvmLoopData uvm)
            forM_ [0..(fromIntegral c-1)] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                r <- peekElemOff resultTable (fromIntegral slot)
                void $ tryPutMVar lock (fromIntegral r)   -- unlock ghc thread with MVar

            return c

--------------------------------------------------------------------------------

-- | 'bracket' wrapper for 'allocSlot/freeSlot'.
--
initUVSlot :: HasCallStack => UVManager -> Resource UVSlot
initUVSlot uvm = initResource (allocSlot uvm) (freeSlot uvm)

-- | Allocate a slot number for given handler or request.
--
allocSlot :: HasCallStack => UVManager -> IO UVSlot
allocSlot uvm@(UVManager blockTableRef freeSlotList loop _ _ _ _ _ _) =
    modifyMVar freeSlotList $ \ freeList -> case freeList of
        (s:ss) -> return (ss, s)
        []     -> withUVManager uvm $ \ _ -> do
            -- free list is empty, we double it
            -- but we shouldn't do it if uv_run doesn't finish yet
            -- because we may re-allocate loop data in another thread
            -- so we take the running lock first

            blockTable <- readIORef blockTableRef
            let oldSiz = sizeofArr blockTable
                newSiz = oldSiz * 2

            blockTable' <- newArr newSiz
            copyArr blockTable' 0 blockTable 0 oldSiz

            forM_ [oldSiz..newSiz-1] $ \ i ->
                writeArr blockTable' i =<< newEmptyMVar
            !iBlockTable' <- unsafeFreezeArr blockTable'

            writeIORef blockTableRef iBlockTable'

            uvLoopResize loop (fromIntegral newSiz)

            return ([fromIntegral oldSiz+1 .. fromIntegral newSiz-1], fromIntegral oldSiz)    -- fill the free slot list


-- | Return slot back to the uv manager where it allocate from.
--
freeSlot :: UVManager -> UVSlot -> IO ()
freeSlot (UVManager _ freeSlotList _ _ _ _ _ _ _) slot =
    modifyMVar_ freeSlotList $ \ freeList -> return (slot:freeList)

--------------------------------------------------------------------------------

-- | Safely lock an uv manager and perform uv_handle initialization.
--
-- Initialization an UV handle usually take two step:
--
--   * allocate an uv_handle struct with proper size
--   * lock a particular uv_loop from a uv manager, and perform custom initialization, such as @uv_tcp_init@.
--
-- And this is what 'initUVHandle' do, all you need to do is to provide the manager you want to hook the handle
-- onto(usually the one on the same capability, i.e. the one obtained by 'getUVManager'),
-- and provide a custom initialization function.
--
initUVHandle :: HasCallStack
         => UVHandleType
         -> (Ptr UVLoop -> Ptr UVHandle -> IO (Ptr UVHandle))
         -> UVManager
         -> Resource (Ptr UVHandle)
initUVHandle typ init uvm = initResource
        (do handle <- hs_uv_handle_alloc typ
            withUVManager uvm (\ loop -> init loop handle) `onException` (hs_uv_handle_free handle)
            return handle
        )
        (hs_uv_handle_close) -- handle is free in uv_close callback

forkBa :: IO () -> IO ThreadId
forkBa io = do
    !uvmArr <- readIORef uvManagerArray
    let !siz = sizeofArr uvmArr
        !firstUVM = indexArr uvmArr 0
    firstIdle <- readIORefU (uvmIdleCounter firstUVM)
    mostIdleUVM <- go uvmArr siz 1 firstIdle firstUVM
    forkOn (uvmCap mostIdleUVM) io
  where
    go :: Array UVManager -> Int -> Int -> Int -> UVManager -> IO UVManager
    go !uvmArr !siz !i !maxIdle !uvm
        | i < siz   = do
            let !uvm' = indexArr uvmArr i
            idle <- readIORefU (uvmIdleCounter uvm')
            if idle >= maxIdle
            then go uvmArr siz (i+1) idle uvm'
            else go uvmArr siz (i+1) maxIdle uvm
        | otherwise = return uvm
