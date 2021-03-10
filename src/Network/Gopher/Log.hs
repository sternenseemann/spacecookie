{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module is completely exposed by 'Network.Gopher'
module Network.Gopher.Log
  ( GopherLogStr ()
  , makeSensitive
  , hideSensitive
  , GopherLogLevel (..)
  , ToGopherLogStr (..)
  , FromGopherLogStr (..)
  ) where

import Network.Gopher.Util (uEncode, uDecode)

import Data.ByteString.Builder (Builder ())
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Sequence as S
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import System.Socket.Family.Inet6

-- | Indicates the log level of a 'GopherLogStr' to a
--   'Network.Gopher.GopherLogHandler'. If you want to
--   filter by log level you can use either the 'Ord'
--   or 'Enum' instance of 'GopherLogLevel' as the following
--   holds:
--
-- @
-- 'GopherLogLevelError' < 'GopherLogLevelWarn' < 'GopherLogLevelInfo'
-- @
data GopherLogLevel
  = GopherLogLevelError
  | GopherLogLevelWarn
  | GopherLogLevelInfo
  deriving (Show, Eq, Ord, Enum)

-- | UTF-8 encoded string which may have parts of it marked as
--   sensitive (see 'makeSensitive'). Use its 'ToGopherLogStr',
--   'Semigroup' and 'IsString' instances to construct
--   'GopherLogStr's and 'FromGopherLogStr' to convert to the
--   commonly used Haskell string types.
newtype GopherLogStr
  = GopherLogStr { unGopherLogStr :: S.Seq GopherLogStrChunk }

instance Show GopherLogStr where
  show = show . (fromGopherLogStr :: GopherLogStr -> String)

instance Semigroup GopherLogStr where
  GopherLogStr s1 <> GopherLogStr s2 = GopherLogStr (s1 <> s2)

instance Monoid GopherLogStr where
  mempty = GopherLogStr mempty

instance IsString GopherLogStr where
  fromString = toGopherLogStr

data GopherLogStrChunk
  = GopherLogStrChunk
  { glscSensitive :: Bool
  , glscBuilder   :: Builder
  }

-- | Mark a 'GopherLogStr' as sensitive. This is used by this
--   library mostly to mark IP addresses of connecting clients.
--   By using 'hideSensitive' on a 'GopherLogStr' sensitive
--   parts will be hidden from the string — even if the sensitive
--   string was concatenated to other strings.
makeSensitive :: GopherLogStr -> GopherLogStr
makeSensitive = GopherLogStr
  . fmap (\c -> c { glscSensitive = True })
  . unGopherLogStr

-- | Replaces all chunks of the 'GopherLogStr' that have been
--   marked as sensitive by 'makeSensitive' with @[redacted]@.
--   Note that the chunking is dependent on the way the string
--   was assembled by the user and the internal implementation
--   of 'GopherLogStr' which can lead to multiple consecutive
--   @[redacted]@ being returned unexpectedly. This may be
--   improved in the future.
hideSensitive :: GopherLogStr -> GopherLogStr
hideSensitive = GopherLogStr
  . fmap (\c -> GopherLogStrChunk False $
      if glscSensitive c
        then BB.byteString "[redacted]"
        else glscBuilder c)
  . unGopherLogStr

-- | Convert 'GopherLogStr's to other string types. Since it is used
--   internally by 'GopherLogStr', it is best to use the 'Builder'
--   instance for performance if possible.
class FromGopherLogStr a where
  fromGopherLogStr :: GopherLogStr -> a

instance FromGopherLogStr GopherLogStr where
  fromGopherLogStr = id

instance FromGopherLogStr Builder where
  fromGopherLogStr = foldMap glscBuilder . unGopherLogStr

instance FromGopherLogStr BL.ByteString where
  fromGopherLogStr = BB.toLazyByteString . fromGopherLogStr

instance FromGopherLogStr B.ByteString where
  fromGopherLogStr = BL.toStrict . fromGopherLogStr

instance FromGopherLogStr T.Text where
  fromGopherLogStr = T.decodeUtf8 . fromGopherLogStr

instance FromGopherLogStr TL.Text where
  fromGopherLogStr = TL.decodeUtf8 . fromGopherLogStr

instance FromGopherLogStr [Char] where
  fromGopherLogStr = uDecode . fromGopherLogStr

-- | Convert something to a 'GopherLogStr'. In terms of
--   performance it is best to implement a 'Builder' for
--   the type you are trying to render to 'GopherLogStr'
--   and then reuse its 'ToGopherLogStr' instance.
class ToGopherLogStr a where
  toGopherLogStr :: a -> GopherLogStr

instance ToGopherLogStr GopherLogStr where
  toGopherLogStr = id

instance ToGopherLogStr Builder where
  toGopherLogStr b = GopherLogStr
    . S.singleton
    $ GopherLogStrChunk
    { glscSensitive = False
    , glscBuilder = b
    }

instance ToGopherLogStr B.ByteString where
  toGopherLogStr = toGopherLogStr . BB.byteString

instance ToGopherLogStr BL.ByteString where
  toGopherLogStr = toGopherLogStr . BB.lazyByteString

instance ToGopherLogStr [Char] where
  toGopherLogStr = toGopherLogStr . uEncode

instance ToGopherLogStr GopherLogLevel where
  toGopherLogStr l =
    case l of
      GopherLogLevelInfo  -> toGopherLogStr ("info"  :: B.ByteString)
      GopherLogLevelWarn  -> toGopherLogStr ("warn"  :: B.ByteString)
      GopherLogLevelError -> toGopherLogStr ("error" :: B.ByteString)

instance ToGopherLogStr (SocketAddress Inet6) where
  -- TODO shorten address if possible
  toGopherLogStr (SocketAddressInet6 addr port _ _) =
    let (b1, b2, b3, b4, b5, b6, b7, b8) = inet6AddressToTuple addr
      in toGopherLogStr $
        BB.charUtf8 '[' <>
        BB.word16HexFixed b1 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b2 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b3 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b4 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b5 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b6 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b7 <> BB.charUtf8 ':' <>
        BB.word16HexFixed b8 <> BB.charUtf8 ']' <>
        BB.charUtf8 ':' <> BB.intDec (fromIntegral port)
