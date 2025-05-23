{-|
Module      : Network.Gopher.Util
Stability   : experimental
Portability : POSIX

Helper utilities used within the library and the server which also could be useful for other application code.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util (
  -- * String Encoding
    uEncode
  , uDecode
  ) where

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import qualified Data.String.UTF8 as U

-- | Encode a 'String' to a UTF-8 'ByteString'
uEncode :: String -> ByteString
uEncode = B.pack . U.encode

-- | Decode a UTF-8 'ByteString' to a 'String'
uDecode :: ByteString -> String
uDecode = fst . U.decode . B.unpack


