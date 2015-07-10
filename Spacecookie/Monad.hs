{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Spacecookie.Monad where

import           Control.Applicative       (Applicative (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (MonadReader (..), ReaderT (..))
import           Network.Socket            (Socket ())
import           Spacecookie.ConfigParsing (Config (..))

-- | GopherdEnv holds the Environment for the Spacecookie
-- application / all functions operating on the Monad.
-- it is the socket used for communicating with the client
-- and the server's config
data GopherdEnv = GopherdEnv { serverSocket :: Socket
                             , serverConfig :: Config
                             }


-- | The Spacecookie Monad is a wrapper around the ReaderT Monad
-- using GopherdEnv as Environment and has Effects to IO
newtype Spacecookie a = Spacecookie { runSpacecookie :: ReaderT GopherdEnv IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader GopherdEnv)
