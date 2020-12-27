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
import Network.Gopher
import Network.Gopher.Util

main = do
  'runGopherPure' ('GopherConfig' "localhost" 7000 Nothing) (\\req -> 'FileResponse' ('uEncode' req))
@

This server just returns the request string as a file.

There are three possibilities for a 'GopherResponse':

* 'FileResponse': file type agnostic file response, takes a 'ByteString' to support both text and binary files
* 'MenuResponse': a gopher menu (“directory listing”) consisting of a list of 'GopherMenuItem's
* 'ErrorResponse': gopher way to show an error (e. g. if a file is not found). A 'ErrorResponse' results in a menu response with a single entry.

If you use 'runGopher', it is the same story like in the example above, but you can do 'IO' effects. To see a more elaborate example, have a look at the server code in this package.

Note: In practice it is probably best to use record update syntax on 'defaultConfig' which won't break your application every time the config record fields are changed.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Gopher (
  -- * Main API
    runGopher
  , runGopherPure
  , runGopherManual
  , GopherConfig (..)
  , defaultConfig
  -- * Helper Functions
  , gophermapToDirectoryResponse
  , setupGopherSocket
  -- * Representations
  -- ** Responses
  , GopherResponse (..)
  , GopherMenuItem (..)
  , GopherFileType (..)
  -- ** Gophermaps
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
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.Log.FastLogger
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import System.Posix.User

-- | necessary information to handle gopher requests
data GopherConfig
  = GopherConfig { cServerName    :: ByteString            -- ^ “name” of the server (either ip address or dns name)
                 , cListenAddr    :: Maybe ByteString      -- ^ Address or hostname to listen on (resolved by @getaddrinfo@).
                                                           --   If 'Nothing', listen on all addresses.
                 , cServerPort    :: Integer               -- ^ port to listen on
                 , cRunUserName   :: Maybe String          -- ^ user to run the process as
                 , cLogConfig     :: Maybe GopherLogConfig -- ^ Can be used to customize the log output.
                                                           --   If 'Nothing', logging is disabled.
                 }

-- | Default 'GopherConfig' describing a server on @localhost:70@ with logging enabled.
defaultConfig :: GopherConfig
defaultConfig = GopherConfig "localhost" Nothing 70 Nothing (Just defaultLogConfig)

data Env
  = Env { serverConfig :: GopherConfig
        , serverFun    :: (String -> IO GopherResponse)
        , logger       :: Maybe (TimedFastLogger, IO ()) -- ^ TimedFastLogger and clean up action
        }

initEnv :: (String -> IO GopherResponse) -> GopherConfig -> IO Env
initEnv fun cfg = do
  timeCache <- newTimeCache simpleTimeFormat
  maybeLogger <-
    if isJust (cLogConfig cfg)
      then Just <$> newTimedFastLogger timeCache (LogStderr 128)
      else pure Nothing
  pure $ Env cfg fun maybeLogger

newtype GopherM a = GopherM { runGopherM :: ReaderT Env IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Env, MonadError IOException)

gopherM :: Env -> GopherM a -> IO a
gopherM env action = (runReaderT . runGopherM) action env

-- This action has become a bit obfuscated. Essentially
-- we initially get the log config and the logger out
-- of Env. Both those values may be Nothing, so we
-- figure out in the Maybe monad:
--
-- * if we have a logger
-- * if we have a log config
-- * if the configured log handler returns a LogStr
--   for the given LogMessage
--
-- If all of those things hold we perform the log
-- action in the IO Monad and get rid of the intermediate
-- Maybe layer.
log :: LogMessage -> GopherM ()
log logMsg = do
  c <- fmap (cLogConfig . serverConfig) ask
  mLogger <- fmap logger ask
  liftIO . fromMaybe (pure ()) $ do
    cfg <- c
    (tlogger, _) <- mLogger
    renderedMsg <- glcLogHandler cfg logMsg
    pure . tlogger $ (\t ->
          let tStr = if glcLogTimed cfg then "[" <> toLogStr t <> "]" else ""
            in tStr <> renderedMsg <> "\n")

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
    env <- initEnv f cfg
    gopherM env $ do
      addr <- liftIO $ getAddress sock
      log $ LogInfoListeningOn addr

      -- Change UID and GID if necessary
      when (isJust (cRunUserName cfg)) $ do
        success <- liftIO . dropPrivileges . fromJust $ cRunUserName cfg
        if success
           then log . LogInfoChangedUser . fromJust $ cRunUserName cfg
           else log . LogErrorCantChangeUid . fromJust $ cRunUserName cfg

      liftIO $ ready

      (forever (acceptAndHandle sock) `catchError`
        (\e -> do
          log . LogErrorAccept $ e
          maybe (pure ()) snd . logger <$> ask >>= liftIO)))

forkGopherM :: GopherM () -> GopherM ThreadId
forkGopherM action = ask >>= liftIO . forkIO . (flip gopherM) action

handleIncoming :: Socket Inet6 Stream TCP -> SocketAddress Inet6 -> GopherM ()
handleIncoming clientSock addr = do
  req <- liftIO $ uDecode . stripNewline <$> receiveRequest clientSock
  log $ LogInfoRequest req addr

  fun <- serverFun <$> ask
  res <- liftIO (fun req) >>= response

  _ <- liftIO $ sendAll clientSock res msgNoSignal
  liftIO $ close clientSock
  log $ LogInfoClosedConnection addr

acceptAndHandle :: Socket Inet6 Stream TCP -> GopherM ()
acceptAndHandle sock = do
  (clientSock, addr) <- liftIO $ accept sock
  log $ LogInfoNewConnection addr
  _ <- forkGopherM $ handleIncoming clientSock addr `catchError` (\e -> do
    liftIO (close clientSock `catchError` const (pure ()))
    log $ LogErrorClosedConnection addr e)
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
