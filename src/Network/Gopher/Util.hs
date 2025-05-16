{-|
Module      : Network.Gopher.Util
Stability   : experimental
Portability : POSIX

Helper utilities used within the library and the server which also could be useful for other application code.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util (
  -- * Security
    sanitizePath
  , dropPrivileges
  -- * String Encoding
  , asciiOrd
  , asciiChr
  , asciiToLower
  , uEncode
  , uDecode
  -- * Misc Helpers
  , stripNewline
  , boolToMaybe
  ) where

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Char (ord, chr, toLower)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())
import System.FilePath.Posix.ByteString (RawFilePath, normalise, joinPath, splitPath, equalFilePath)
import System.Posix.User

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

-- | Strip @\\r@ and @\\n@ from 'ByteString's
stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s           = B.empty
  | B.head s `elem`
    (map (fromIntegral . ord) "\n\r") = stripNewline (B.tail s)
  | otherwise          = B.head s `B.cons` stripNewline (B.tail s)

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
sanitizePath :: RawFilePath -> RawFilePath
sanitizePath =
  -- To retain prior behavior @"."@ after normalisation is mapped to @""@
  (\p -> if p == "." then "" else p)
  . joinPath
  . filter (\p -> not (equalFilePath p ".."))
  . splitPath . normalise

-- | prop> boolToMaybe True x == Just x
--   prop> boolToMaybe False x == Nothing
boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True  a = Just a
boolToMaybe False _ = Nothing

-- | Call 'setGroupID' and 'setUserID' to switch to
--   the given user and their primary group.
--   Requires special privileges.
--   Will raise an exception if either the user
--   does not exist or the current user has no
--   permission to change UID/GID.
dropPrivileges :: String -> IO ()
dropPrivileges username = do
  user <- getUserEntryForName username
  setGroupID $ userGroupID user
  setUserID $ userID user
