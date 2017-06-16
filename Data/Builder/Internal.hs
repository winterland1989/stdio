{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Data.Builder.Internal where

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word
import Data.Bits (shiftL)
import Data.Primitive
import Debug.Trace
import Control.Monad.ST.Unsafe
import System.IO.Unsafe (unsafeInterleaveIO)

-- | 'AllocateStrategy' will decide how each 'BuildStep' proceed when previous buffer is not enough.
--
data AllocateStrategy m
    = DoubleBuffer       -- Double the buffer and continue building
    | InsertChunk        -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> m ())   -- Freeze current chunk and perform action with it.
                                        -- Use the 'V.Bytes' argument outside the action is dangerous
                                        -- since we will reuse the buffer after action finished.

-- | Helper type to help ghc unpack
--
data Buffer s = Buffer {-# UNPACK #-} !(A.MutablePrimArray s Word8)  -- well, the buffer content
                       {-# UNPACK #-} !Int  -- writing offset

-- | @BuilderStep@ is a function that fill buffer under given conditions.
--
type BuildStep m =
       AllocateStrategy m                    -- see 'AllocateStrategy'
    -> Buffer (PrimState m)
    -> m [V.Bytes]

-- | @Builder@ is a monoid to help compose @BuilderStep@. With next @BuilderStep@ continuation,
-- We can do interesting things like perform some action, or interleave the build process.
--
newtype Builder = Builder { runBuilder :: BuildStep IO -> BuildStep IO }

#if MIN_VERSION_base(4,9,0)
instance Semigroup Builder where
   (<>) = append
   {-# INLINE (<>) #-}
#endif

instance Monoid Builder where
   mempty  = empty
   {-# INLINE mempty #-}
#if MIN_VERSION_base(4,9,0)
   mappend = (<>) -- future-proof definition
#else
   mappend = append
#endif
   {-# INLINE mappend #-}
   mconcat = foldr append empty
   {-# INLINE mconcat #-}

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE append #-}

empty :: Builder
empty = Builder id
{-# INLINE empty #-}


-- | A builder that modify the resulting list of chunk.
modifyChunks :: ([V.Bytes] -> [V.Bytes]) -> Builder
modifyChunks f = Builder (\ k strategy buffer -> f `fmap` (k strategy buffer))
{-# INLINE modifyChunks #-}

writeN :: Int -> (A.MutablePrimArray (PrimState IO) Word8 -> Int -> IO ()) -> Builder
writeN n f = ensureFree n `append`
    Builder (\ k strategy (Buffer buf offset ) ->
        f buf offset >> k strategy (Buffer buf (offset+n))
    )
{-# INLINE writeN #-}

-- | Ensure that there are at least @n@ many elements available.
ensureFree :: Int -> Builder
ensureFree !n = Builder $ \ k strategy buffer@(Buffer buf offset) -> do
    let siz = A.sizeofMutableArr buf  -- You may think doing this will be slow
                                      -- but this value lives in CPU cache for most of the time
    if siz - offset >= n
    then k strategy buffer
    else (runBuilder flush) k strategy buffer
{-# INLINE ensureFree #-}

-- | /O(1)./ Pop the strict @Bytes@ we have constructed so far, if any.
--
--
flush :: Builder
flush = Builder $ \ k strategy buffer@(Buffer buf offset) ->
    let siz = A.sizeofMutableArr buf
    in case strategy of
        DoubleBuffer -> do
            buf' <- A.resizeMutableArr buf ((siz + V.chunkOverhead) `shiftL` 1 - V.chunkOverhead)   -- double the buffer
            k strategy (Buffer buf' offset)          -- continue building

        OneShotAction action -> do
            arr <- A.unsafeFreezeArr buf             -- popup a copy
            action (V.PrimVector arr 0 offset)
            k strategy (Buffer buf 0)           -- continue building with old buf

        InsertChunk -> do
            arr <- A.unsafeFreezeArr buf       -- popup a copy
            buf' <- A.newArr siz               -- make a new buffer
            xs <- unsafeInterleaveIO (k strategy (Buffer buf' 0 ))  -- delay the rest building process
            let v = V.fromArr arr 0 offset
            v `seq` return (v : xs)

{-# NOINLINE flush #-} -- We really don't want to bloat our code with bound handling.

word8 :: Word8 -> Builder
word8 x = writeN 1 $ \ !marr !o -> A.writeArr marr o x
{-# INLINE word8 #-}

buildBytes :: Builder -> V.Bytes
buildBytes (Builder b) = runST $ unsafeIOToST $ do
    buf <- A.newArr V.defaultInitSize
    [bs] <- b lastStep DoubleBuffer (Buffer buf 0 )
    return bs
  where
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINE buildBytes #-}

buildBytesList :: Builder -> [V.Bytes]
buildBytesList (Builder b) = runST $ unsafeIOToST $ do
    buf <- A.newArr V.smallChunkSize
    b lastStep InsertChunk (Buffer buf 0)
  where
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        return [V.PrimVector arr 0 offset]
{-# INLINE buildBytesList #-}

buildAndRun :: (V.Bytes -> IO ()) -> Builder -> IO ()
buildAndRun action (Builder b) = do
    buf <- A.newArr V.defaultChunkSize
    _ <- b lastStep (OneShotAction action) (Buffer buf 0)
    return ()
  where
    lastStep _ (Buffer buf offset) = do
        arr <- A.unsafeFreezeArr buf
        action (V.PrimVector arr 0 offset)
        return [] -- to match the silly return type
{-# INLINE buildAndRun #-}

--------------------------------------------------------------------------------

data BoundedBuilder = BoundedBuilder {-# UNPACK #-} !Int (A.PrimArray Word8 -> IO Int)

--------------------------------------------------------------------------------

class Build a => BoundedBuild a where
    fbuild :: a -> BoundedBuilder

class Build a where
    build :: a -> Builder

