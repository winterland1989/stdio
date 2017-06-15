
-- | A @Builder@ is an efficient way to build lazy @Text@ values.
-- There are several functions for constructing builders, but only one
-- to inspect them: to extract any data, you have to turn them into
-- lazy @Text@ values using @toLazyText@.
--
-- Internally, a builder constructs a lazy @Text@ by filling arrays
-- piece by piece.  As each buffer is filled, it is \'popped\' off, to
-- become a new chunk of the resulting lazy @Text@.  All this is
-- hidden from the user of the @Builder@.
newtype Builder = Builder {
     -- Invariant (from Data.Text.Lazy):
     --      The lists include no null Texts.
     runBuilder :: forall s.
            (MutablePrimArray s Word8 -> Int -> ST s [Bytes])
            -> MutablePrimArray s Word8
            -> Int
            -> ST s [Bytes]
   }

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


-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | A builder that modify the resulting list of chunk.
modifyChunks :: ([Bytes] -> [Bytes]) -> Builder
modifyChunks f = Builder (\ k buf free -> f `fmap` (k buf free))

-- | /O(1)./ The empty @Builder@, satisfying
--
--  * @'runBuilder' 'empty' = 'V.empty'@
--
empty :: Builder
empty = Builder (\ k buf free -> k buf free)
{-# INLINE empty #-}

-- | Ensure that there are at least @n@ many elements available.
ensureFree :: Int -> Builder
ensureFree !n = Builder $ \ k buf free ->
    if free >= n
    then k buf free
    else flush

-- | /O(1)./ Pop the strict @Bytes@ we have constructed so far, if any.
--
flush :: Builder
flush = Builder $ \ k buf free -> do
    let siz = sizeofMutablePrimArray
    if siz == free  -- buffer is unused, no need to flush
    then k buf free
    else
        let l = siz - free
        arr <- A.freezeArr 0 l buf       -- popup a copy
        ts <- inlineInterleaveST (k buf s)
        return (V.fromArr arr 0 l) : ts
{-# INLINE [1] flush #-}

--------------------------------------------------------------------------------

data BoundedBuilder = BoundedBuilder {-# UNPACK #-} !Int (PrimArray Word8 -> IO Int)

--------------------------------------------------------------------------------

class Build a => BoundedBuild a where
    fbuild :: a -> BoundedBuilder

class Build a where
    build :: a -> Builder


writeN :: Int -> (forall s. A.MArray s -> Int -> ST s ()) -> Builder
writeN n f = writeAtMost n (\ p o -> f p o >> return n)
{-# INLINE writeN #-}


fromText :: S.Text -> Builder
fromText t@(Text arr off l)
    | S.null t       = empty
    | l <= copyLimit = writeN l $ \marr o -> A.copyI marr o arr off (l+o)
    | otherwise      = flush `append` mapBuilder (t :)
{-# INLINE [1] fromText #-}

