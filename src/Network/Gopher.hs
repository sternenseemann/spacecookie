{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Gopher
Stability   : experimental
Portability : POSIX

= Overview

This is the main module of the spacecookie library. It allows to write gopher applications by taking care of handling gopher requests while leaving the application logic to a user-supplied function.

For a small tutorial an example of a trivial pure gopher application:

@
{-# LANGUAGE OverloadedStrings #-}
import "Network.Gopher"
import "Network.Gopher.Util"

cfg :: 'GopherConfig'
cfg = 'defaultConfig'
  { cServerName = "localhost"
  , cServerPort = 7000
  }

main :: IO ()
main = 'runGopherPure' cfg (\\req -> 'FileResponse' ('uEncode' req))
@

This server just returns the request string as a file.

There are three possibilities for a 'GopherResponse':

* 'FileResponse': file type agnostic file response, takes a 'ByteString' to support both text and binary files
* 'MenuResponse': a gopher menu (“directory listing”) consisting of a list of 'GopherMenuItem's
* 'ErrorResponse': gopher way to show an error (e. g. if a file is not found). A 'ErrorResponse' results in a menu response with a single entry.

If you use 'runGopher', it is the same story like in the example above, but you can do 'IO' effects. To see a more elaborate example, have a look at the server code in this package.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Gopher (
  -- * Main API
    runGopher
  , runGopherPure
  , runGopherManual
  , GopherConfig (..)
  , defaultConfig
  -- ** Responses
  , GopherResponse (..)
  , GopherMenuItem (..)
  , GopherFileType (..)
  -- * Helper Functions
  -- ** Logging
  -- $loggingDoc
  , GopherLogHandler
  , module Network.Gopher.Log
  -- ** Networking
  , setupGopherSocket
  -- ** Gophermaps
  , gophermapToDirectoryResponse
  , Gophermap
  , GophermapEntry (..)
  ) where

import Prelude hiding (log)

import Network.Gopher.Log
import Network.Gopher.Types
import Network.Gopher.Util
import Network.Gopher.Util.Gophermap

import Control.Concurrent (forkIO, ThreadId ())
import Control.Exception (throw, bracket, IOException ())
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad.Reader (ask, runReaderT, MonadReader (..), ReaderT (..))
import Control.Monad.Error.Class (MonadError (..))
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import System.Posix.User

-- | Necessary information to handle gopher requests
data GopherConfig
  = GopherConfig
  { cServerName    :: ByteString
  -- ^ Public name of the server (either ip address or dns name).
  --   Gopher clients will use this name to fetch any resources
  --   listed in gopher menus located on the same server.
  , cListenAddr    :: Maybe ByteString
  -- ^ Address or hostname to listen on (resolved by @getaddrinfo@).
  --   If 'Nothing', listen on all addresses.
  , cServerPort    :: Integer
  -- ^ Port to listen on
  , cRunUserName   :: Maybe String
  -- ^ User to run the process as
  , cLogHandler    :: Maybe GopherLogHandler
  -- ^ 'IO' action spacecookie will call to output its log messages.
  --   If it is 'Nothing', logging is disabled. See [the logging section](#logging)
  --   for an overview on how to implement a log handler.
  }

-- | Default 'GopherConfig' describing a server on @localhost:70@ with
--   no registered log handler.
defaultConfig :: GopherConfig
defaultConfig = GopherConfig "localhost" Nothing 70 Nothing Nothing

-- | Type for an user defined 'IO' action which handles logging a
--   given 'GopherLogStr' of a given 'GopherLogLevel'. It may
--   process the string and format in any way desired, but it must
--   be thread safe and should not block (too long) since it
--   is called syncronously.
type GopherLogHandler = GopherLogLevel -> GopherLogStr -> IO ()

-- $loggingDoc
-- #logging#
-- Logging may be enabled by providing 'GopherConfig' with an optional
-- 'GopherLogHandler' which implements processing, formatting and
-- outputting of log messages. While this requires extra work for the
-- library user it also allows the maximum freedom in used logging
-- mechanisms.
--
-- A trivial log handler could look like this:
--
-- @
-- logHandler :: 'GopherLogHandler'
-- logHandler level str = do
--   putStr $ show level ++ \": \"
--   putStrLn $ 'fromGopherLogStr' str
-- @
--
-- If you only want to log errors you can use the 'Ord' instance of
-- 'GopherLogLevel':
--
-- @
-- logHandler' :: 'GopherLogHandler'
-- logHandler' level str = when (level <= 'GopherLogLevelError')
--   $ logHandler level str
-- @
--
-- The library marks parts of 'GopherLogStr' which contain user
-- related data like IP addresses as sensitive using 'makeSensitive'.
-- If you don't want to e. g. write personal information to disk in
-- plain text, you can use 'hideSensitive' to transparently remove
-- that information. Here's a quick example in GHCi:
--
-- >>> hideSensitive $ "Look at my " <> makeSensitive "secret"
-- "Look at my [redacted]"

data Env
  = Env
  { serverConfig :: GopherConfig
  , serverFun    :: (String -> IO GopherResponse)
  }

newtype GopherM a = GopherM { runGopherM :: ReaderT Env IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Env, MonadError IOException)

gopherM :: Env -> GopherM a -> IO a
gopherM env action = (runReaderT . runGopherM) action env

-- call given log handler if it is Just
logIO :: Maybe GopherLogHandler -> GopherLogLevel -> GopherLogStr -> IO ()
logIO h l = fromMaybe (const (pure ())) $ ($ l) <$> h

logInfo :: GopherLogStr -> GopherM ()
logInfo = log GopherLogLevelInfo

logError :: GopherLogStr -> GopherM ()
logError = log GopherLogLevelError

log :: GopherLogLevel -> GopherLogStr -> GopherM ()
log l m = do
  h <- cLogHandler . serverConfig <$> ask
  liftIO $ logIO h l m

receiveRequest :: Socket Inet6 Stream TCP -> IO ByteString
receiveRequest sock = receiveRequest' mempty
  where lengthLimit = 1024
        receiveRequest' acc = do
          bs <- liftIO $ receive sock lengthLimit mempty
          case (B.elemIndex (asciiOrd '\n') bs) of
            Just i -> return (acc `B.append` (B.take (i + 1) bs))
            Nothing -> if B.length bs < lengthLimit
                         then return (acc `B.append` bs)
                         else receiveRequest' (acc `B.append` bs)

dropPrivileges :: String -> IO Bool
dropPrivileges username = do
  uid <- getRealUserID
  if (uid /= 0)
     then return False
     else do
       user <- getUserEntryForName username
       setGroupID $ userGroupID user
       setUserID $ userID user
       return True

-- | Auxiliary function that sets up the listening socket for
--   'runGopherManual' correctly and starts to listen.
setupGopherSocket :: GopherConfig -> IO (Socket Inet6 Stream TCP)
setupGopherSocket cfg = do
  sock <- (socket :: IO (Socket Inet6 Stream TCP))
  setSocketOption sock (ReuseAddress True)
  setSocketOption sock (V6Only False)
  addr <-
    case cListenAddr cfg of
      Nothing -> pure
        $ SocketAddressInet6 inet6Any (fromInteger (cServerPort cfg)) 0 0
      Just a -> do
        let port = uEncode . show $ cServerPort cfg
        let flags = aiV4Mapped <> aiNumericService
        addrs <- (getAddressInfo (Just a) (Just port) flags :: IO [AddressInfo Inet6 Stream TCP])

        -- should be done by getAddressInfo already
        when (null addrs) $ throw eaiNoName

        pure . socketAddress $ head addrs
  bind sock addr
  listen sock 5
  pure sock

-- | Run a gopher application that may cause effects in 'IO'.
--   The application function is given the gopher request (path)
--   and required to produce a GopherResponse.
runGopher :: GopherConfig -> (String -> IO GopherResponse) -> IO ()
runGopher cfg f = runGopherManual (setupGopherSocket cfg) (pure ()) close cfg f

-- | Same as 'runGopher', but allows you to setup the 'Socket' manually
--   and calls an action of type @IO ()@ as soon as the server is ready
--   to accept requests. When the server terminates, it calls the action
--   of type @Socket Inet6 Stream TCP -> IO ()@ to clean up the socket.
--
--   Spacecookie assumes the 'Socket' is properly set up to listen on the
--   port and host specified in the 'GopherConfig' (i. e. 'bind' and
--   'listen' have been called). This can be achieved using 'setupGopherSocket'.
--
--   This is intended for supporting systemd socket activation and storage.
--   Only use if you know what you are doing.
runGopherManual :: IO (Socket Inet6 Stream TCP) -> IO () -> (Socket Inet6 Stream TCP -> IO ())
                -> GopherConfig -> (String -> IO GopherResponse) -> IO ()
runGopherManual sockAction ready term cfg f = bracket
  sockAction
  term
  (\sock -> do
    gopherM (Env cfg f) $ do
      addr <- liftIO $ getAddress sock
      logInfo $ "Listening on " <> toGopherLogStr addr

      -- Change UID and GID if necessary
      case cRunUserName cfg of
        Nothing -> pure ()
        Just u -> do
          success <- liftIO $ dropPrivileges u
          if success
            then logInfo $ "Changed to user " <> toGopherLogStr u
            else logError $ "Can' change to user " <> toGopherLogStr u
            -- TODO: abort?

      liftIO $ ready

      -- TODO exception
      (forever (acceptAndHandle sock) `catchError`
        (\e -> do
          logError $ "Error while accepting new connection: "
            <> toGopherLogStr (show e))))

forkGopherM :: GopherM () -> GopherM ThreadId
forkGopherM action = ask >>= liftIO . forkIO . (flip gopherM) action

handleIncoming :: Socket Inet6 Stream TCP -> SocketAddress Inet6 -> GopherM ()
handleIncoming clientSock addr = do
  req <- liftIO $ uDecode . stripNewline <$> receiveRequest clientSock
  logInfo $ "New Request \"" <> toGopherLogStr req <> "\" from "
    <> makeSensitive (toGopherLogStr addr)

  fun <- serverFun <$> ask
  res <- liftIO (fun req) >>= response

  _ <- liftIO $ sendAll clientSock res msgNoSignal
  liftIO $ close clientSock
  logInfo $ "Closed Connection to " <> makeSensitive (toGopherLogStr addr)

acceptAndHandle :: Socket Inet6 Stream TCP -> GopherM ()
acceptAndHandle sock = do
  (clientSock, addr) <- liftIO $ accept sock
  logInfo $ "New connection from " <> makeSensitive (toGopherLogStr addr)
  _ <- forkGopherM $ handleIncoming clientSock addr `catchError` (\e -> do
    liftIO (close clientSock `catchError` const (pure ()))
    logError $ "Closed connection to " <> makeSensitive (toGopherLogStr addr)
      <> " after error:" <> toGopherLogStr (show e))
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
                     B.concat [ title, uEncode "\t", uEncode path, uEncode "\t", fromMaybe (cServerName (serverConfig env)) host,
                                uEncode "\t", uEncode . show $ fromMaybe (cServerPort (serverConfig env)) port, uEncode "\r\n" ])
              B.empty items

response (FileResponse str) = pure str
response (ErrorResponse reason) = response . MenuResponse $
    [ Item Error (uEncode reason) "Err" Nothing Nothing ]
