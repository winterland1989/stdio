{-# LANGUAGE TupleSections #-}

module Data.Array.LiteralQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Word
import GHC.Types
import GHC.Prim
import Control.Monad
import Data.Char (ord)
import Data.Bits

asciiLiteral :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
asciiLiteral k str = k (length str) $ (LitE . StringPrimL) `fmap` check str
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            reportError $ "character '" ++ [c] ++ "' is have out of range in ASCII literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')

asciiLiteralMulLine :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
asciiLiteralMulLine k str = k (length str) $ (LitE . StringPrimL) `fmap` check str
  where
    check :: String -> Q [Word8]
    check [] = return []
    check (c:cs) = do
        when (ord c > 0xFF) $
            reportWarning $ "character '" ++ [c] ++ "' is out of ASCII range in literal:" ++ str
        cs' <- check cs
        return (fromIntegral (ord c):cs')


utf8Literal :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
utf8Literal = undefined

--------------------------------------------------------------------------------

vectorLiteral :: ([Integer] -> Q [Word8]) -> (Int -> ExpQ -> ExpQ) -> String -> ExpQ
vectorLiteral f k str = do
    (len, ws) <- parse str
    k len $ (return . LitE . StringPrimL) ws
  where
    parse :: String -> Q (Int, [Word8])
    parse str = do
        case (readList :: ReadS [Integer]) ("[" ++ str ++ "]") of
            [(is, "")] -> (length is, ) `fmap` f is
            _ -> do reportError $ "can't parse vector literal:" ++ str
                    return (0, [])

word16LiteralLE :: (Int -> ExpQ -> ExpQ) -> String -> ExpQ
word16LiteralLE k str = vectorLiteral mkWord16LE k str
  where
    mkWord16LE :: [Integer] -> Q [Word8]
    mkWord16LE [] = return []
    mkWord16LE (i:is) = do
        when (i<0 || i > 0xFFFF) $
            reportError $ "integer " ++ show i ++ " is out of word16 range in literal:" ++ str
        ws <- mkWord16LE is
        let w1 = fromIntegral (i .&. 0xFF)
            w2 = fromIntegral (i `shiftR` 8 .&. 0xFF)
        return (fromIntegral w1:w2:ws)



