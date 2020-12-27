{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Config (..)
  ) where

import Control.Monad (mzero, join)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.ByteString (ByteString ())
import Network.Gopher.Util

data Config = Config { serverName    :: ByteString
                     , listenAddr    :: Maybe ByteString
                     , serverPort    :: Integer
                     , runUserName   :: Maybe String
                     , rootDirectory :: FilePath
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "hostname" <*>
    (v .:? "listen" >>= fmap join . traverse (.:? "addr")) <*>
    ((v .: "listen" >>= (.: "port")) <|> v .:? "port" .!= 70) <*>
    v .:? "user" <*>
    v .: "root"
  parseJSON _ = mzero

instance ToJSON Config where
  toJSON (Config host addr port user root) = object $
    [ "hostname" .= host
    , "listen" .= listenObj
    , "root" .= root
    ] ++ maybeBind "user" user
    where listenObj = object $ maybeBind "addr" addr ++ [ "port" .= port ]
          maybeBind n v = maybe [] ((:[]) . (n .=)) v

-- auxiliary instances for types that have no default instance
instance FromJSON ByteString where
  parseJSON s@(String _) = uEncode <$> parseJSON s
  parseJSON _ = mzero

instance ToJSON ByteString where
  toJSON = toJSON . uDecode
