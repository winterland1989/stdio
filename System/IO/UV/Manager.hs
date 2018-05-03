{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

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
  , uvmCap
  , getUVManager
  , lockUVManager
  , lockUVManager'
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

#define UV_IDLE_LIMIT 20

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmLoop        :: {-# UNPACK #-} !(Ptr UVLoop)                 -- the uv loop refrerence

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
    uvm == uvm' = uvmCap uvm == uvmCap uvm'

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    s <- newQSemN 0
    forM [0..numCaps-1] $ \ i -> do
        -- fork uv manager thread
        forkOn i . withResource (initUVManager i) $ \ m -> do
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


initUVManager :: HasCallStack => Int -> Resource UVManager
initUVManager cap = do
    loop  <- initUVLoop (fromIntegral cap)
    async <- initUVAsyncWake loop
    timer <- initUVTimer loop

    liftIO $ do

        running <- newMVar False
        idleCounter <- newCounter UV_IDLE_LIMIT

        return (UVManager loop running async timer idleCounter cap)

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- libuv is not thread safe, use this function to perform any action which will mutate uv_loop's state.
--
lockUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
lockUVManager uvm f = do

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
        _       -> yield >> lockUVManager uvm f -- we yield here, because uv_run is probably not finished yet

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- Some action did not request uv_loop pointer explicitly, but will mutate uv_loop underhood, for example:
-- @uv_read_start@. These actions have to be protected by locking the uv_loop.
--
-- In fact most of the libuv's functions are not thread safe, and 'uv_run' can be running concurrently
-- without this lock.
--
lockUVManager' :: HasCallStack => UVManager -> IO a -> IO a
lockUVManager' uvm f = lockUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ running _ _ _ _) = loop -- use a closure capture uvm in case of stack memory leaking
  where
    loop = do
        e <- withMVar running $ \ _ -> step uvm False   -- we borrow mio's non-blocking/blocking poll strategy here
        if e > 0                                        -- first we do a non-blocking poll, if we got events
        then yield >> loop                              -- we yield here, to let other threads do actual work
        else do                                         -- otherwise we still yield once
            yield                                       -- in case other threads can still progress
            e <- withMVar running $ \ _ -> step uvm False   -- now we do another non-blocking poll to make sure
            if e > 0 then yield >> loop             -- if we got events somehow, we yield and go back
            else do                                 -- if there's still no events, we directly jump to safe blocking poll

                _ <- swapMVar running True          -- after swap this lock, other thread can wake up us
                e <- step uvm True                  -- by send async handler, and it's thread safe
                _ <- swapMVar running False

                yield                               -- we yield here, to let other threads do actual work
                loop

    -- call uv_run, return the event number
    step :: UVManager -> Bool -> IO CSize
    step (UVManager loop _ _ timer _ _) block = do
            clearUVLoopData loop        -- clean event counter

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

            peekUVLoopData loop

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
             -> Resource (Ptr UVHandle, Ptr UVContext)
initUVHandle typ init uvm = initResource
    (do handle <- throwOOMIfNull $ hs_uv_handle_alloc typ
        lockUVManager uvm (\ loop -> init loop handle) `onException` (hs_uv_handle_close handle)
        dat <- peekUVHandleContext handle
        return (handle, dat)
    )
    (\ (handle, _) -> hs_uv_handle_close handle) -- handle is free in uv_close callback

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
