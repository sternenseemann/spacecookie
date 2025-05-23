{-|
Module      : Network.Gopher.Util
Stability   : experimental
Portability : POSIX

Helper utilities used within the library and the server which also could be useful for other application code.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util (
  -- * String Encoding
    asciiOrd
  , asciiChr
  , asciiToLower
  , uEncode
  , uDecode
  ) where

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Char (ord, chr, toLower)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())

-- | 'chr' a 'Word8'
asciiChr :: Word8 -> Char
asciiChr = chr . fromIntegral

-- | 'ord' a 'Word8'
asciiOrd :: Char -> Word8
asciiOrd = fromIntegral . ord

-- | Transform a 'Word8' to lowercase if the solution is in bounds.
asciiToLower :: Word8 -> Word8
asciiToLower w =
  if inBounds lower
    then fromIntegral lower
    else w
  where inBounds i = i >= fromIntegral (minBound :: Word8) &&
          i <= fromIntegral (maxBound :: Word8)
        lower :: Int
        lower = ord . toLower . asciiChr $ w

-- | Encode a 'String' to a UTF-8 'ByteString'
uEncode :: String -> ByteString
uEncode = B.pack . U.encode

-- | Decode a UTF-8 'ByteString' to a 'String'
uDecode :: ByteString -> String
uDecode = fst . U.decode . B.unpack


