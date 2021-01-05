{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Config (..)
  , LogConfig (..)
  ) where

import Control.Monad (mzero, join)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.ByteString (ByteString ())
import Data.Text (toLower)
import Network.Gopher.Log (LogLevel (..))
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

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "hostname"
    <*> (v .:? "listen" >>= fmap join . traverse (.:? "addr"))
    <*> ((v .: "listen" >>= (.: "port")) <|> v .:? "port" .!= 70)
    <*> v .:? "user"
    <*> v .: "root"
    <*> v .:? "log" .!= defaultLogConfig
  parseJSON _ = mzero

data LogConfig
  = LogConfig
  { logEnable   :: Bool
  , logHideIps  :: Bool
  , logHideTime :: Bool
  , logLevel    :: LogLevel
  }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig True True False LogLevelInfo

instance FromJSON LogConfig where
  parseJSON (Object v) = LogConfig
    <$> v .:?  "enable"   .!= logEnable defaultLogConfig
    <*> v .:? "hide-ips"  .!= logHideIps defaultLogConfig
    <*> v .:? "hide-time" .!= logHideTime defaultLogConfig
    <*> v .:? "level"     .!= logLevel defaultLogConfig
  parseJSON _ = mzero

-- auxiliary instances for types that have no default instance
instance FromJSON LogLevel where
  parseJSON (String s) =
    case toLower s of
      "info"  -> pure LogLevelInfo
      "error" -> pure LogLevelError
      _ -> mzero
  parseJSON _ = mzero

instance FromJSON ByteString where
  parseJSON s@(String _) = uEncode <$> parseJSON s
  parseJSON _ = mzero

instance ToJSON ByteString where
  toJSON = toJSON . uDecode
