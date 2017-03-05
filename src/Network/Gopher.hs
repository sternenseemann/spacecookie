{-|
Module      : Network.Gopher
Stability   : experimental
Portability : POSIX

= Overview

This is the main module of the spacecookie library. It allows to write gopher applications by taking care of handling gopher requests while leaving the application logic to a user-supplied function.

For a small tutorial an example of a trivial pure gopher application:

@
{-# LANGUAGE OverloadedStrings #-}
import Network.Gopher
import Network.Gopher.Util

main = do
  'runGopherPure' ('GopherConfig' "localhost" 7000 Nothing) (\\req -> 'FileResponse' ('uEncode' req))
@

This server just returns the request string as a file.

There are three possibilities for a 'GopherResponse':

* 'FileResponse': file type agnostic file response, takes a 'ByteString' to support both text and binary files
* 'MenuResponse': a gopher menu (“directory listning”) consisting of a list of 'GopherMenuItem's
* 'ErrorResponse': gopher way to show an error (e. g. if a file is not found). A 'ErrorResponse' results in a menu response with a single entry.

If you use 'runGopher', it is the same story like in the example above, but you can do 'IO' effects. To see a more elaborate example, have a look at the server code in this package.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Gopher (
  -- * Main API
    runGopher
  , runGopherPure
  , GopherConfig (..)
  -- * Helper Functions
  , gophermapToDirectoryResponse
  -- * Representations
  -- ** Responses
  , GopherResponse (..)
  , GopherMenuItem (..)
  , GopherFileType (..)
  -- ** Gophermaps
  , GophermapEntry (..)
  , Gophermap (..)
  ) where

import Network.Gopher.Types
import Network.Gopher.Util
import Network.Gopher.Util.Gophermap

import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Maybe (isJust, fromJust, fromMaybe)
import Control.Applicative ((<$>), (<*>), Applicative (..))
import Control.Concurrent (forkIO, ThreadId ())
import Control.Exception (bracket, catch)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad.Reader (ask, runReaderT, MonadReader (..), ReaderT (..))
import qualified Data.String.UTF8 as U
import System.IO
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import System.Posix.User

-- | necessary information to handle gopher requests
data GopherConfig
  = GopherConfig { cServerName    :: ByteString   -- ^ “name” of the server (either ip address or dns name)
                 , cServerPort    :: Integer      -- ^ port to listen on
                 , cRunUserName   :: Maybe String -- ^ user to run the process as
                 }

data Env
  = Env { serverSocket :: Socket Inet6 Stream TCP
        , serverName   :: ByteString
        , serverPort   :: Integer
        , serverFun    :: (String -> IO GopherResponse)
        }

newtype GopherM a = GopherM { runGopherM :: ReaderT Env IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Env)

gopherM env action = (runReaderT . runGopherM) action env

receiveRequest :: Socket Inet6 Stream TCP -> IO ByteString
receiveRequest sock = receiveRequest' sock mempty
  where lengthLimit = 1024
        receiveRequest' sock acc = do
          bs <- liftIO $ receive sock lengthLimit mempty
          case (B.elemIndex (asciiOrd '\n') bs) of
            Just i -> return (acc `B.append` (B.take (i + 1) bs))
            Nothing -> if B.length bs < lengthLimit
                         then return (acc `B.append` bs)
                         else receiveRequest' sock (acc `B.append` bs)

dropPrivileges :: String -> IO ()
dropPrivileges username = do
  uid <- getRealUserID
  when (uid /= 0) $ return ()

  user <- getUserEntryForName username
  setGroupID $ userGroupID user
  setUserID $ userID user

-- | Run a gopher application that may cause effects in 'IO'.
--   The application function is given the gopher request (path)
--   and required to produce a GopherResponse.
runGopher :: GopherConfig -> (String -> IO GopherResponse) -> IO ()
runGopher cfg f = bracket
  (socket :: IO (Socket Inet6 Stream TCP))
  close
  (\sock -> do
    setSocketOption sock (ReuseAddress True)
    setSocketOption sock (V6Only False)
    bind sock (SocketAddressInet6 inet6Any (fromInteger (cServerPort cfg)) 0 0)
    listen sock 5

    -- Change UID and GID if necessary
    when (isJust (cRunUserName cfg)) $ dropPrivileges (fromJust (cRunUserName cfg))

    let env = Env sock (cServerName cfg) (fromInteger (cServerPort cfg)) f

    gopherM env $ forever (acceptAndHandle sock))

forkGopherM :: GopherM () -> GopherM ThreadId
forkGopherM action = ask >>= liftIO . forkIO . (flip gopherM) action

acceptAndHandle :: Socket Inet6 Stream TCP -> GopherM ()
acceptAndHandle sock = do
  (clientSock, _) <- liftIO $ accept sock
  forkGopherM $ do
    req <- liftIO $ uDecode . stripNewline <$> receiveRequest clientSock

    fun <- serverFun <$> ask
    res <- liftIO (fun req) >>= response

    liftIO $ sendAll clientSock res msgNoSignal
    liftIO $ close clientSock
  return ()

-- | Run a gopher application that may not cause effects in 'IO'.
runGopherPure :: GopherConfig -> (String -> GopherResponse) -> IO ()
runGopherPure cfg f = runGopher cfg (fmap pure f)

response :: GopherResponse -> GopherM ByteString
response (MenuResponse items) = do
  env <- ask
  pure $ foldl (\acc (Item fileType title path host port) ->
                 B.append acc $
                   fileTypeToChar fileType `B.cons`
                     B.concat [ title, uEncode "\t", uEncode path, uEncode "\t", fromMaybe (serverName env) host,
                                uEncode "\t", uEncode . show $ fromMaybe (serverPort env) port, uEncode "\r\n" ])
              B.empty items

response (FileResponse str) = pure str
response (ErrorResponse reason) = do
  env <- ask
  pure $ fileTypeToChar Error `B.cons`
    B.concat [uEncode reason, uEncode $  "\tErr\t", serverName env, uEncode "\t", uEncode . show $ serverPort env, uEncode "\r\n"]
