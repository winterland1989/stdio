
-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more inp data
--
-- * success: the remaining unparsed data and the parser value
--
data Result a
    = Success !Bytes a
    | Failure !Bytes [String]
    | NeedMore (Maybe Bytes -> Result a)

instance Functor Result where
    fmap f (Success s a)   = Success s (f a)
    fmap _ (Failure s msg) = Failure s msg
    fmap f (NeedMore k) = NeedMore (fmap f . k)

instance Show a => Show (Result a) where
    show (Failure errs) = "ParseFailure: " <> show err
    show (ParseMore _)  = "ParseMore _"
    show (Success a)    = "ParseOK " <> show a <> " " <> show b

-- | Simple parser structure
newtype Parser a = Parser
    { runParser :: forall r . Bytes
                -> (Bytes -> a -> Result inp r) -- The success continuation
                -> Result inp r            -- We don't need failure continuation
    }                                             -- since the failure is written on the ParseResult tag


instance Monad Get where
  return = pure
  {-# INLINE return #-}
  Parser pa >>= f = Parser (\ inp k -> pa inp (\ inp' a -> runParser (f a) inp' k))
  {-# INLINE (>>=) #-}
#if MIN_VERSION_base(4,9,0)
  fail = Fail.fail
instance Fail.MonadFail Get where
#endif
  fail str = Parser (\ inp _ -> Failure inp [str])
  {-# INLINE fail #-}

instance Applicative Get where
    pure x = Parser (\ inp k -> k inp x)
    {-# INLINE pure #-}
    Parser pf (<*>) Parser pa = do { x1 <- m1; x2 <- m2; return (x1 x2) }
    {-# INLINE (<*>) #-}

instance Functor Get where
    fmap f (Parser pa) = Parser (\ inp k -> pa inp (\ inp' a -> k inp' (f a)))
    {-# INLINE fmap #-}
    a <$ Parser pb = Parser (\ inp k -> pb inp (\ inp' _ -> k inp' a))
    {-# INLINE (<$) #-}

instance MonadPlus Get where
    mzero = empty
    mplus = (<|>)

instance Alternative Get where
    empty = Parser (\ inp _ -> Failure inp "Data.Parser(Alternative).empty")
    {-# INLINE empty #-}
    f <|> g = do
        (r, bs) <- runAndKeepTrack f
        case r of
            Success inp x -> Parser (\ _ k -> k inp x)
            Failure _ _ -> pushBack bs >> g
            _ -> error "Binary: impossible"
    {-# INLINE (<|>) #-}

-- | Run a parser and keep track of all the inp it consumes.
-- Once it's finished, return the final result (always 'Success' or 'Failure') and
-- all consumed chunks.
--
runAndKeepTrack :: Get a -> Get (Result a, [Bytes])
runAndKeepTrack (Parser pa) = Parser $ \ inp k ->
    let r0 = pa inp (\ inp' a -> Success inp' a) in go [] r0
  where
    go !acc r = case r of
        NeedMore k -> NeedMore (\ minp -> go (maybe acc (:acc) minp) (k minp))
        _          -> k inp (r, reverse acc)
{-# INLINE runAndKeepTrack #-}

pushBack :: [Bytes] -> Result ()
pushBack [] = Parser (\ inp k -> k inp ())
pushBack bs = Parser (\ inp k -> k (B.concat (inp : bs)) ())
{-# INLINE pushBack #-}

parse :: Bytes -> Parser a -> Result a
parse input (Parser p) = p input (\ input' a -> Success input' a)

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Get ()
ensureN !n0 = C $ \inp ks -> do
  if B.length inp >= n0
    then ks inp ()
    else runCont (withInputChunks n0 enoughChunks onSucc onFail >>= put) inp ks
  where -- might look a bit funny, but plays very well with GHC's inliner.
        -- GHC won't inline recursive functions, so we make ensureN non-recursive
    enoughChunks n str
      | B.length str >= n = Right (str,B.empty)
      | otherwise = Left (n - B.length str)
    -- Sometimes we will produce leftovers lists of the form [B.empty, nonempty]
    -- where `nonempty` is a non-empty ByteString. In this case we can avoid a copy
    -- by simply dropping the empty prefix. In principle ByteString might want
    -- to gain this optimization as well
    onSucc = B.concat . dropWhile B.null
    onFail bss = C $ \_ _ -> Fail (B.concat bss) "not enough bytes"
