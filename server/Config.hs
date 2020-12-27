{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Config (..)
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString ())
import Network.Gopher.Util

data Config = Config { serverName    :: ByteString
                     , serverPort    :: Integer
                     , runUserName   :: Maybe String
                     , rootDirectory :: FilePath
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "hostname" <*>
    v .: "port" <*>
    v .:? "user" <*>
    v .: "root"
  parseJSON _ = mzero

instance ToJSON Config where
  toJSON (Config host port user root) = object $
    [ "hostname" .= host
    , "port" .= port
    , "root" .= root
    ] ++
    maybe [] ((:[]) . ("user" .=)) user

-- auxiliary instances for types that have no default instance
instance FromJSON ByteString where
  parseJSON s@(String _) = uEncode <$> parseJSON s
  parseJSON _ = mzero

instance ToJSON ByteString where
  toJSON = toJSON . uDecode
