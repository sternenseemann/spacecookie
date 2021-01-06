{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

data GopherLogLevel
  = GopherLogLevelError
  | GopherLogLevelInfo
  deriving (Show, Eq, Ord)

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

makeSensitive :: GopherLogStr -> GopherLogStr
makeSensitive = GopherLogStr
  . fmap (\c -> c { glscSensitive = True })
  . unGopherLogStr

hideSensitive :: GopherLogStr -> GopherLogStr
hideSensitive = GopherLogStr
  . fmap (\c -> GopherLogStrChunk False $
      if glscSensitive c
        then BB.byteString "[redacted]"
        else glscBuilder c)
  . unGopherLogStr

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
