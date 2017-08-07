{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.CBytes where

import Foreign.C
import Data.Primitive.PrimArray
import Control.Monad.Primitive
import Data.Foldable (foldlM)
import Data.IORef
import Data.Word
import Data.String (IsString(..))
import Data.Text.UTF8Codec (encodeCBytesChar)
import qualified Data.Vector as V
import GHC.CString
import GHC.Stack.Compat
import GHC.Ptr
import Control.Monad
import Control.Monad.ST
import Data.Bits
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified System.IO.Exception as E

-- | A efficient wrapper for null-terminated string which can be automatically freed
-- by ghc garbage collector.
--
-- The main design target of this type is to ease the bridging of C FFI APIs, since most
-- of the unix APIs use null-terminated string. On windows you're encouraged to use a
-- compatibility layer like 'WideCharToMultiByte/MultiByteToWideChar' and keep the same
-- interface.
--
-- We neither store length info, nor support O(1) slice for 'CBytes': This will defeat the
-- purpose of null-terminated string which is to save memory, in practice this is not an
-- issue: most 'CBytes's are very small, e.g. filepath, hostname, etc. And 'strlen' runs
-- very fast.
--
-- It can be used with @OverloadedString@, literal encoding is UTF-8 with some modifications:
-- @\NUL@ char is encoded to 'C0 80', and '\xD800' ~ '\xDFFF' is encoded as a three bytes
-- normal utf-8 codepoint. This is also how ghc compile string literal into binaries,
-- thus we can use rewrite-rules to construct 'CBytes' value in O(1) without wasting runtime heap.
--
-- Note most of the unix API is not unicode awared though, you may find a `scandir` call
-- return a filename which is not proper encoded in any unicode encoding at all.
-- But still, UTF-8 is recommanded to be used everywhere, so we use that assumption in
-- various places, such as displaying and converting literals.
--
data CBytes
    = CBytesOnHeap  {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)   -- ^ On heap pinned 'MutablePrimArray'
    | CBytesPtr     {-# UNPACK #-} !CString  {-# UNPACK #-} !(IORef ())  -- ^ C pointers with finalizer
    | CBytesLiteral {-# UNPACK #-} !CString                              -- ^ String literals with static address

instance Show CBytes where
    show = unpackCBytes

instance Read CBytes where
    readsPrec p s = [(packCBytes x, r) | (x, r) <- readsPrec p s]

instance Eq CBytes where
    cbyteA == cbyteB = unsafeDupablePerformIO $
        withCBytes cbyteA $ \ pA ->
        withCBytes cbyteB $ \ pB ->
            if pA == pB
            then return True
            else do
                r <- c_strcmp pA pB
                return (r == 0)

instance Ord CBytes where
    cbyteA `compare` cbyteB = unsafeDupablePerformIO $
        withCBytes cbyteA $ \ pA ->
        withCBytes cbyteB $ \ pB ->
            if pA == pB
            then return EQ
            else do
                r <- c_strcmp pA pB
                return (r `compare` 0)

instance IsString CBytes where
    {-# INLINE fromString #-}
    fromString = packCBytes

{-# RULES
    "CBytes packCBytes/unpackCString#" forall addr# .
        packCBytes (unpackCString# addr#) = CBytesLiteral (Ptr addr#)
 #-}
{-# RULES
    "CBytes packCBytes/unpackCStringUtf8#" forall addr# .
        packCBytes (unpackCStringUtf8# addr#) = CBytesLiteral (Ptr addr#)
 #-}

-- | Pack a 'String' into null-terminated 'CByte'.
--
packCBytes :: String -> CBytes
{-# NOINLINE [1] packCBytes #-}
packCBytes s = unsafeDupablePerformIO $ do
    mba <- newPrimArray V.defaultInitSize
    (SP2 i mba') <- foldlM go (SP2 0 mba) s
    writePrimArray mba' i 0     -- the null terminator
    shrinkMutablePrimArray mba' (i+1)
    return (CBytesOnHeap mba')
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 -> Char -> IO SP2
    go (SP2 i mba) !c = do
        siz <- sizeofMutablePrimArray mba
        if i < siz - 4  -- we need at least 5 bytes for safety due to extra '\0' byte
        then do
            i' <- encodeCBytesChar mba i c
            return (SP2 i' mba)
        else do
            let !siz' = siz `shiftL` 1
            !mba' <- resizeMutablePrimArray mba siz'
            i' <- encodeCBytesChar mba' i c
            return (SP2 i' mba')


data SP2 = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray RealWorld Word8)

unpackCBytes :: CBytes -> String
{-# INLINABLE unpackCBytes #-}
unpackCBytes cbytes = unsafeDupablePerformIO . withCBytes cbytes $ \ (Ptr addr#) ->
    return (unpackCStringUtf8# addr#)

--------------------------------------------------------------------------------

-- | Wrap a 'CString' type into a 'CBytes', return Nothing if the pointer is NULL.
--
-- The first argument is a free function to free the 'CString' pointers, e.g. memory allocated
-- by @malloc@ can be automatically freed by 'free'
--
fromCString :: (CString -> IO ())        -- finalizer
            -> CString
            -> IO (Maybe CBytes)
{-# INLINABLE fromCString #-}
fromCString free cstring = do
    if cstring == nullPtr
    then return Nothing
    else do
        ref <- newIORef ()
        _ <- mkWeakIORef ref (free cstring)
        return (Just $ CBytesPtr cstring ref)

-- | Same with 'fromCString', but throw 'E.InvalidArgument' when meet a null pointer.
--
-- The first argument is a function to free the 'CString' pointers, e.g. memory allocated
-- by @malloc@ can be automatically freed by 'free'
--
fromCStringThrow :: HasCallStack
                 => (CString -> IO ())   -- finalizer
                 -> CString
                 -> IO CBytes
{-# INLINABLE fromCStringThrow #-}
fromCStringThrow free cstring = do
    if cstring == nullPtr
    then E.throwIO (E.InvalidArgument
        (E.IOEInfo E.NoErrno "unexpected null pointer" "constructing a CBytes" callStack))
    else do
        ref <- newIORef ()
        _ <- mkWeakIORef ref (free cstring)
        return (CBytesPtr cstring ref)

-- | Pass 'CBytes' to foreign function as a @char*@.
--
withCBytes :: CBytes -> (CString -> IO a) -> IO a
{-# INLINABLE withCBytes #-}
withCBytes (CBytesOnHeap mba) f = withMutablePrimArrayContents mba (f . castPtr)
withCBytes (CBytesPtr ptr ref) f = do { r <- f ptr; touch ref; return r }
withCBytes (CBytesLiteral ptr) f = f ptr

--------------------------------------------------------------------------------

foreign import ccall unsafe "cstring.h strcmp"
    c_strcmp :: CString -> CString -> IO CInt
