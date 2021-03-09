{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.Config
  ( Config (..)
  , LogConfig (..)
  ) where

import Control.Monad (mzero, join)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser ())
import Data.ByteString (ByteString ())
import Data.Text (toLower, Text ())
import Network.Gopher (GopherLogLevel (..))
import Network.Gopher.Util

data Config
  = Config
  { serverName    :: ByteString
  , listenAddr    :: Maybe ByteString
  , serverPort    :: Integer
  , runUserName   :: Maybe String
  , rootDirectory :: FilePath
  , logConfig     :: LogConfig
  }

maybePath :: FromJSON a => [Text] -> Object -> Parser (Maybe a)
maybePath []     _ = fail "got empty path"
maybePath [x]    v = v .:? x
maybePath (x:xs) v = v .:? x >>= fmap join . traverse (maybePath xs)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "hostname"
    <*> maybePath [ "listen", "addr" ] v
    <*> parseListenPort v .!= 70
    <*> v .:? "user"
    <*> v .: "root"
    <*> v .:? "log" .!= defaultLogConfig
  parseJSON _ = mzero

-- Use '(<|>)' over the 'Maybe's in the parser rather
-- to only fallback on 'Nothing' and not on @empty@.
-- This way a parse error in listen → port doesn't get
-- promoted to just 'Nothing'.
parseListenPort :: Object -> Parser (Maybe Integer)
parseListenPort v = (<|>)
  <$> maybePath [ "listen", "port" ] v
  <*> (v .:? "port")

data LogConfig
  = LogConfig
  { logEnable   :: Bool
  , logHideIps  :: Bool
  , logHideTime :: Bool
  , logLevel    :: GopherLogLevel
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig True True False GopherLogLevelInfo

instance FromJSON LogConfig where
  parseJSON (Object v) = LogConfig
    <$> v .:?  "enable"   .!= logEnable defaultLogConfig
    <*> v .:? "hide-ips"  .!= logHideIps defaultLogConfig
    <*> v .:? "hide-time" .!= logHideTime defaultLogConfig
    <*> v .:? "level"     .!= logLevel defaultLogConfig
  parseJSON _ = mzero

-- auxiliary instances for types that have no default instance
instance FromJSON GopherLogLevel where
  parseJSON (String s) =
    case toLower s of
      "info"  -> pure GopherLogLevelInfo
      "warn" -> pure GopherLogLevelWarn
      "error" -> pure GopherLogLevelError
      _ -> mzero
  parseJSON _ = mzero

instance FromJSON ByteString where
  parseJSON s@(String _) = uEncode <$> parseJSON s
  parseJSON _ = mzero

instance ToJSON ByteString where
  toJSON = toJSON . uDecode
