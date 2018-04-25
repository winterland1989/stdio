{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
  , peekBufferTable
  , withUVManager
  , withUVManager'
  , initUVHandle
  , initUVReq
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

#define IDLE_LIMIT 20

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable   :: {-# UNPACK #-} !(IORef (UnliftedArray (MVar ()))) -- a array to store threads blocked on async I/O
                                                                            -- Int inside MVar is the async action's result.

    , uvmLoop        :: {-# UNPACK #-} !(Ptr UVLoop)        -- the uv loop refrerence
    , uvmLoopData    :: {-# UNPACK #-} !(Ptr UVLoopData)    -- cached pointer to uv_loop_t's data field

    , uvmRunning     :: {-# UNPACK #-} !(MVar Bool)     -- only uv manager thread will modify this value
                                                        -- 'True' druing uv_run and 'False' otherwise.
                                                        --
                                                        -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                        -- we have to wake up the loop before mutating uv_loop's
                                                        -- state.

    , uvmAsync       :: {-# UNPACK #-} !(Ptr UVHandle)  -- This async handle is used when we want to break from
                                                        -- a blocking uv_run, send async request is the only
                                                        -- thread safe wake up mechanism for libuv.


    , uvmTimer       :: {-# UNPACK #-} !(Ptr UVHandle)  -- This timer handle is used to achieve polling with
                                                        -- given timeout

    , uvmIdleCounter :: {-# UNPACK #-} !(Counter)       -- Counter for idle(no event) uv_runs, when counter
                                                        -- reaches IDLE_LIMIT, we start to increase waiting between
                                                        -- uv_run, until delay reach 8 milliseconds

    , uvmCap ::  {-# UNPACK #-} !Int                -- the capability uv manager should run on, we save this
                                                    -- number for starting uv manager with `forkOn`
    }

instance Show UVManager where
    show uvm = "UVManager on capability " ++ show (uvmCap uvm)

instance Eq UVManager where
    uvm == uvm' =
        uvmCap uvm == uvmCap uvm'

initTableSize :: Int
initTableSize = 64

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
getBlockMVar :: UVManager -> Int -> IO (MVar ())
{-# INLINABLE getBlockMVar #-}
getBlockMVar uvm slot = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable slot

-- | Poke a prepared buffer and size into loop data under given slot.
--
-- NOTE, this action is not protected with 'withUVManager' for effcient reason, you should merge this action
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
pokeBufferTable :: UVManager -> Int -> Ptr Word8 -> Int -> IO ()
{-# INLINABLE pokeBufferTable #-}
pokeBufferTable uvm slot buf bufSiz = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    pokeElemOff bufTable slot buf
    pokeElemOff bufSizTable slot (fromIntegral bufSiz)

peekBufferTable :: UVManager -> Int -> IO Int
{-# INLINABLE peekBufferTable #-}
peekBufferTable uvm slot = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    fromIntegral <$> (peekElemOff bufSizTable slot :: IO CSsize)

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

        loopData <- peekUVLoopData loop

        running <- newMVar False

        idleCounter <- newCounter IDLE_LIMIT

        return (UVManager blockTableRef loop loopData running async timer idleCounter cap)

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
-- In fact most of the libuv's functions are not thread safe, so watch out!
--
withUVManager' :: HasCallStack => UVManager -> IO a -> IO a
withUVManager' uvm f = withUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ _  _ running _ _ idleCounter _) = do

    e <- withMVar running $ \ _ -> step uvm False   -- we borrow mio's non-blocking/blocking poll strategy here
    if e > 0
    then yield >> startUVManager uvm       -- we yield here, to let other threads do actual work
    else do
        yield
        e <- withMVar running $ \ _ -> step uvm False   -- we do another non-blocking poll to make sure
        if e > 0 then yield >> startUVManager uvm
        else do                                 -- if there's still no events, we directly jump to safe blocking poll

            _ <- swapMVar running True          -- after changing this, other thread can wake up us
            e <- step uvm True                  -- by send async handler, and it's thread safe
            _ <- swapMVar running False

            yield                               -- we yield here, to let other threads do actual work
            startUVManager uvm

  where

    -- call uv_run, return the event number
    step :: UVManager -> Bool -> IO CSize
    step (UVManager blockTableRef loop loopData _ _ timer _ _) block = do
            blockTable <- readIORef blockTableRef
            clearUVEventCounter loopData        -- clean event counter

            if block
            then if rtsSupportsBoundThreads
                then do
                    uvTimerStop timer
                    void $ uvRunSafe loop uV_RUN_ONCE
                else do
                    -- use a 2ms timeout blocking poll on non-threaded rts
                    uvTimerWakeStart timer 2
                    void $ uvRun loop uV_RUN_ONCE
            else do
                -- we use uV_RUN_ONCE with 1ms timeout instead of uV_RUN_NOWAIT
                -- because we don't want to spin CPU cycles on GHC scheduler too much
                -- and 1ms is the shortest timeout we can achieve with libuv's timer
                void $ uvRun loop uV_RUN_NOWAIT

            (c, q) <- peekUVEventQueue loopData
            forM_ [0..(fromIntegral c-1)] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable (fromIntegral slot)
                void $ tryPutMVar lock ()   -- unlock ghc thread with MVar
            return c

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
initUVHandle typ init uvm =
    initResource
        (withUVManager uvm $ \ loop -> do
            handle <- throwOOMIfNull $ hs_uv_handle_alloc loop typ

            slot <- peekUVHandleData handle
            blockTable <- readIORef (uvmBlockTable uvm)
            when (slot == sizeofArr blockTable) $ do
                let newSiz = slot * 2
                blockTable' <- newArr newSiz
                copyArr blockTable' 0 blockTable 0 slot
                !iBlockTable' <- unsafeFreezeArr blockTable'
                writeIORef (uvmBlockTable uvm) iBlockTable'

            init loop handle `onException` hs_uv_handle_free handle
            return handle)
        (withUVManager' uvm . hs_uv_handle_close) -- handle is free in uv_close callback

initUVReq :: HasCallStack
          => UVReqType
          -> UVManager
          -> Resource (Ptr UVReq)
initUVReq typ uvm =
    initResource
        (withUVManager uvm $ \ loop -> do
            req <- throwOOMIfNull (hs_uv_req_alloc loop typ)

            slot <- peekUVReqData req
            blockTable <- readIORef (uvmBlockTable uvm)
            when (slot == sizeofArr blockTable) $ do
                let newSiz = slot * 2
                blockTable' <- newArr newSiz
                copyArr blockTable' 0 blockTable 0 slot
                !iBlockTable' <- unsafeFreezeArr blockTable'
                writeIORef (uvmBlockTable uvm) iBlockTable'

            return req)
        (\ req -> withUVManager uvm $ \ loop -> hs_uv_req_free loop req)

-- | Fork a new GHC thread with active load-balancing.
--
-- Using libuv based I/O solution has a disadvantage that file handlers are bound to certain
-- uv_loop, thus certain uv mananger/capability. This makes GHC's work-stealing strategy diffcult
-- to work, we solve this problem with simple round-robin load-balancing: forkBa will automatically
-- distribute your new threads in round-robin manner.
--
forkBa :: IO () -> IO ThreadId
forkBa io = do
    i <- atomicAddCounter_ counter 1
    forkOn i io
  where
    counter :: Counter
    {-# NOINLINE counter #-}
    counter = unsafePerformIO $ newCounter 0
