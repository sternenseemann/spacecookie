{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util
  ( stripNewline
  , santinizePath
  , asciiOrd
  , asciiChr
  , uEncode
  , uDecode
  ) where

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Char (ord, chr)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())
import System.FilePath.Posix (pathSeparator, normalise, joinPath, splitPath)

asciiChr :: Word8 -> Char
asciiChr = chr . fromIntegral

asciiOrd :: Char -> Word8
asciiOrd = fromIntegral . ord

uEncode :: String -> ByteString
uEncode = B.pack . U.encode

uDecode :: ByteString -> String
uDecode = fst . U.decode . B.unpack

-- | Strip \r and \n from ByteStrings
stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s           = B.empty
  | B.head s `elem`
    (map (fromIntegral . ord) "\n\r") = stripNewline (B.tail s)
  | otherwise          = B.head s `B.cons` stripNewline (B.tail s)

-- | Normalize the path and prevent path traversal
--   attacks.
santinizePath :: FilePath -> FilePath
santinizePath path = joinPath . filter (\p -> p /= ".." && p /= ".") . splitPath . normalise $ path
