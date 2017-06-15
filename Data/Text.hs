module Data.Text where


-- | 'Text' represented as UTF-8 encoded 'Bytes'
--
newtype Text = Text Bytes

type ErrMsg = Text

validateUTF8 :: Bytes -> (Bool, Int)

fromUTF8 :: Bytes -> (Text, Bytes)
fromUTF8Lenient :: Bytes -> (Text, Bytes)
toUTF8 :: Text -> Bytes

fromUTF16 :: Bytes -> (Text, Bytes)
fromUTF16Lenient :: Bytes -> (Text, Bytes)
toUTF16 :: Text -> Bytes





decodeChar :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# INLINEABLE decodeChar #-}
decodeChar addr idx =
    case cnt of
        0# -> (# chr# (word2Int# x1), 1# #)
        1# ->
            let !x2 = indexWord8OffAddr# addr (idx +# 1#)
            in if validate2 x1 x2
                then (# chr2 x1 x2, 2# #)
                else (# '\0'#, 0# #)
        2# ->
            let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                !x3 = indexWord8OffAddr# addr (idx +# 2#)
            in if validate3 x1 x2 x3
                then (# chr3 x1 x2 x3, 3# #)
                else (# '\0'#, 0# #)
        3# ->
            let !x2 = indexWord8OffAddr# addr (idx +# 1#)
                !x3 = indexWord8OffAddr# addr (idx +# 2#)
                !x4 = indexWord8OffAddr# addr (idx +# 3#)
            in if validate4 x1 x2 x3 x4
                then (# chr4 x1 x2 x3 x4, 4# #)
                else (# '\0'#, 0# #)
        _  -> (# '\0'#, 0# #)

{-# INLINEABLE encodeChar #-}

encodeChar _ = charUtf8
