
-- | This is data structure attach to uv_loop_t 's data field. It should be mirrored
-- to c struct in hs_uv.c.
--
data UVLoopData = UVLoopData
    { uVLoopEventCounter :: CSize                       -- These two fields compose a special data structure
    , uvLoopEventQueue   :: Ptr CSize                   -- to keep trace of events during a uv_run
                                                        -- before each uv_run the counter should be cleared
                                                        --
    , uvLoopReadBufferTable  :: Ptr (Ptr Word8)         -- a list to keep read buffer's refrerence
    , uvLoopReadBufferSizeTable :: Ptr CSize            -- a list to keep read buffer's size
    , uvLoopWriteBufferTable  :: Ptr (Ptr Word8)        -- a list to keep write buffer's refrerence
    , uvLoopWriteBufferSizeTable  :: Ptr CSize          -- a list to keep write buffer's size
    , uvLoopResultTable  :: Ptr CSize                   -- a list to keep callback's return value
                                                        -- such as file or read bytes number
    } deriving Show

instance Storable UVLoopData where
    sizeOf _ = sizeOf (undefined :: Word) * 7
    alignment _ = alignment (undefined :: Word)
    poke p (UVLoopData p1 p2 p3 p4 p5 p6 p7) = do
        let pp = castPtr p
        pokeElemOff pp 0 p1
        pokeElemOff pp 1 p2
        pokeElemOff pp 2 p3
        pokeElemOff pp 3 p4
        pokeElemOff pp 4 p5
        pokeElemOff pp 5 p6
        pokeElemOff pp 6 p7

    peek p =
        let pp = castPtr p
        in UVLoopData
            <$> peekElemOff pp 0
            <*> peekElemOff pp 1
            <*> peekElemOff pp 2
            <*> peekElemOff pp 3
            <*> peekElemOff pp 4
            <*> peekElemOff pp 5
            <*> peekElemOff pp 6

peekUVLoopuEvent :: Ptr UVLoopData -> IO (Int, Ptr Int)
peekUVLoopuEvent p = do
    let pp = castPtr p
    c <- peekElemOff pp 0 :: IO CSize
    q <- peekElemOff pp 1
    return (fromIntegral c, q)

peekResultTable :: Ptr UVLoopData -> IO (Ptr CSize)
peekResultTable p = do
    let pp = castPtr p
    peekElemOff pp 6

peekReadBuffer :: Ptr UVLoopData -> IO (Ptr (Ptr Word8), Ptr CSize)
peekReadBuffer p = do
    let pp = castPtr p
    b <- peekElemOff pp 2
    s <- peekElemOff pp 3
    return (castPtr b, s)


clearUVLoopuEventCounter :: Ptr UVLoopData -> IO ()
clearUVLoopuEventCounter p = let pp = castPtr p in poke pp (0 :: CSize)
