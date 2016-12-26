{-|
Module      : Network.Gopher.Util
Stability   : experimental
Portability : POSIX

Helper utilities used within the library and the server which also could be useful for other application code.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util (
  -- * Security
    santinizePath
  , santinizeIfNotUrl
  -- * String Encoding
  , asciiOrd
  , asciiChr
  , uEncode
  , uDecode
  -- * Misc Helpers
  , stripNewline
  ) where

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Char (ord, chr)
import Data.List (isPrefixOf)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())
import System.FilePath.Posix (pathSeparator, normalise, joinPath, splitPath)

-- | 'chr' a 'Word8'
asciiChr :: Word8 -> Char
asciiChr = chr . fromIntegral

-- | 'ord' a 'Word8'
asciiOrd :: Char -> Word8
asciiOrd = fromIntegral . ord

-- | Encode a 'String' to a UTF-8 'ByteString'
uEncode :: String -> ByteString
uEncode = B.pack . U.encode

-- | Decode a UTF-8 'ByteString' to a 'String'
uDecode :: ByteString -> String
uDecode = fst . U.decode . B.unpack

-- | Strip @\\r@ and @\\n@ from 'ByteString's
stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s           = B.empty
  | B.head s `elem`
    (map (fromIntegral . ord) "\n\r") = stripNewline (B.tail s)
  | otherwise          = B.head s `B.cons` stripNewline (B.tail s)

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
santinizePath :: FilePath -> FilePath
santinizePath path = joinPath . filter (\p -> p /= ".." && p /= ".") . splitPath . normalise $ path

santinizeIfNotUrl :: FilePath -> FilePath
santinizeIfNotUrl path = if "URL:" `isPrefixOf` path
                           then path
                           else santinizePath path
