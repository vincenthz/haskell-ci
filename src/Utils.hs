{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Utils
    ( Digest
    , SHA256
    , readHciHash
    , quitWith
    , yamlAutoGeneratedComment
    , yamlIsAutogeneratedComment
    , yamlGetAutogeneratedHash
    , escapeQuote
    , lastMaybe
    ) where

import           Crypto.Hash (hashWith, SHA256(..), Digest, digestFromByteString)
import           Basement.From
import           Data.ByteArray.Encoding

import qualified Foundation    as F
import qualified Foundation.IO as F
import           Foundation.Collection (isPrefixOf, isSuffixOf)
import           System.IO
import           System.Exit
import           Prelude

yamlAutoGeneratedComment :: Digest SHA256 -> String
yamlAutoGeneratedComment hash = yamlAutoGeneratedCommentPrefix ++ show hash ++ yamlAutoGeneratedCommentSuffix

yamlAutoGeneratedCommentPrefix = "# ~*~ auto-generated by haskell-ci with config : "
yamlAutoGeneratedCommentSuffix = " ~*~"

yamlIsAutogeneratedComment s =
    isSuffixOf yamlAutoGeneratedCommentSuffix s && isPrefixOf yamlAutoGeneratedCommentPrefix s

yamlGetAutogeneratedHash :: String -> Maybe (Digest SHA256)
yamlGetAutogeneratedHash s
    | yamlIsAutogeneratedComment s =
        let r = dropFromEnd (length yamlAutoGeneratedCommentSuffix) $ drop (length yamlAutoGeneratedCommentPrefix) s
        in if length r == 64 && all isHex r
                then case fmap (digestFromByteString @SHA256 @(F.UArray F.Word8)) $ convertFromBase  Base16 $ into @(F.UArray F.Word8) @F.String $ F.fromList r of
                        Right (Just d)  -> Just d
                        _ -> Nothing
                else Nothing
    | otherwise =
        Nothing
  where
    dropFromEnd n = reverse . drop n . reverse
    isHex :: Char -> Bool
    isHex = flip elem ("0123456789abcdefABCDEF" :: [Char])

readHciHash :: IO (Digest SHA256)
readHciHash = hashWith SHA256 <$> F.readFile ".haskell-ci"

quitWith :: String -> IO a
quitWith m = hPutStrLn stderr ("error: " ++ m) >> exitFailure

escapeQuote :: String -> String
escapeQuote [] = []
escapeQuote (x:xs)
    | x == '"'  = '\\' : '"' : escapeQuote xs
    | otherwise = x : escapeQuote xs

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe l  = Just $ last l