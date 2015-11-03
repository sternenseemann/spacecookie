{-# LANGUAGE OverloadedStrings #-}
module Spacecookie.ConfigParsing where
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.ByteString     (ByteString ())
import           Data.Text           (Text ())
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import           Data.Yaml           hiding (Result (..))
import           Network.Socket      (PortNumber ())

-- | The config holds some simple parameters that modify
-- the behavior of the server.
data Config = Config { serverName    :: ByteString
                     , serverPort    :: PortNumber
                     , runUserName   :: ByteString
                     , rootDirectory :: FilePath
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "hostname" <*>
    v .: "port" <*>
    v .: "user" <*>
    v .: "root"
  parseJSON _ = mzero

instance ToJSON Config where
  toJSON (Config host port user root) = object ["hostname" .= host, "port" .= port, "user" .= user, "root" .= root]

-- auxiliary instances for types that have no default instance
instance FromJSON ByteString where
  parseJSON (String s) = encodeUtf8 <$> (parseJSON (String s) :: Parser Text)
  parseJSON _ = mzero

instance FromJSON PortNumber where
  parseJSON (Number port) = fromIntegral <$> (parseJSON (Number port) :: Parser Integer)
  parseJSON _ = mzero

instance ToJSON ByteString where
  toJSON str = toJSON $ decodeUtf8 str

instance ToJSON PortNumber where
  toJSON port = toJSON (fromIntegral port :: Integer)
