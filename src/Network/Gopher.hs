{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Gopher
Stability   : experimental
Portability : POSIX

= Overview

This is the main module of the spacecookie library.
It allows to write gopher applications by taking care of
handling gopher requests while leaving the application
logic to a user-supplied function.

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

handler :: 'GopherRequest' -> 'GopherResponse'
handler request =
  case 'requestSelector' request of
    "hello" -> 'FileResponse' "Hello, stranger!"
    "" -> rootMenu
    "/" -> rootMenu
    _ -> 'ErrorResponse' "Not found"
  where rootMenu = 'MenuResponse'
          [ 'Item' 'File' "greeting" "hello" Nothing Nothing ]

main :: IO ()
main = 'runGopherPure' cfg handler
@

There are three possibilities for a 'GopherResponse':

* 'FileResponse': file type agnostic file response, takes a
  'ByteString' to support both text and binary files.
* 'MenuResponse': a gopher menu (“directory listing”) consisting of a
  list of 'GopherMenuItem's
* 'ErrorResponse': gopher way to show an error (e. g. if a file is not found).
  An 'ErrorResponse' results in a menu response with a single entry.

If you use 'runGopher', it is the same story like in the example above, but
you can do 'IO' effects. To see a more elaborate example, have a look at the
server code in this package.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Gopher (
  -- * Main API
  -- $runGopherVariants
    runGopher
  , runGopherPure
  , runGopherManual
  , GopherConfig (..)
  , defaultConfig
  -- ** Requests
  , GopherRequest (..)
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
  -- $gophermapDoc
  , gophermapToDirectoryResponse
  , Gophermap
  , GophermapEntry (..)
  ) where

import Prelude hiding (log)

import Network.Gopher.Log
import Network.Gopher.Types
import Network.Gopher.Util
import Network.Gopher.Util.Gophermap
import Network.Gopher.Util.Socket

import Control.Concurrent (forkIO, ThreadId (), threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (bracket, catch, throw, SomeException (), Exception ())
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad.Reader (ask, runReaderT, MonadReader (..), ReaderT (..))
import Data.Bifunctor (second)
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Word (Word16 ())
import System.Socket hiding (Error (..))
import System.Socket.Family.Inet6
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

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
  , cLogHandler    :: Maybe GopherLogHandler
  -- ^ 'IO' action spacecookie will call to output its log messages.
  --   If it is 'Nothing', logging is disabled. See [the logging section](#logging)
  --   for an overview on how to implement a log handler.
  }

-- | Default 'GopherConfig' describing a server on @localhost:70@ with
--   no registered log handler.
defaultConfig :: GopherConfig
defaultConfig = GopherConfig "localhost" Nothing 70 Nothing

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

-- $gophermapDoc
-- Helper functions for converting 'Gophermap's into 'MenuResponse's.
-- For parsing gophermap files, refer to "Network.Gopher.Util.Gophermap".

data GopherRequest
  = GopherRequest
  { requestRawSelector  :: ByteString
  -- ^ raw selector sent by the client (without the terminating @\\r\\n@
  , requestSelector     :: ByteString
  -- ^ only the request selector minus the search expression if present
  , requestSearchString :: Maybe ByteString
  -- ^ raw search string if the clients sends a search transaction
  , requestClientAddr   :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
  -- ^ IPv6 address of the client which sent the request. IPv4 addresses are
  --   <https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses mapped>
  --   to an IPv6 address.
  } deriving (Show, Eq)

data Env
  = Env
  { serverConfig :: GopherConfig
  , serverFun    :: GopherRequest -> IO GopherResponse
  }

newtype GopherM a = GopherM { runGopherM :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

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

logException :: Exception e => Maybe GopherLogHandler -> GopherLogStr -> e -> IO ()
logException logger msg e =
  logIO logger GopherLogLevelError $ msg <> toGopherLogStr (show e)

-- | Read request from a client socket.
--   The complexity of this function is caused by the
--   following design features:
--
--   * Requests may be terminated by either "\n\r" or "\n"
--   * After the terminating newline no extra data is accepted
--   * Give up on waiting on a request from the client after
--     a certain amount of time (request timeout)
--   * Don't accept selectors bigger than a certain size to
--     avoid DoS attacks filling up our memory.
receiveRequest :: Socket Inet6 Stream TCP -> IO (Either ByteString ByteString)
receiveRequest sock = fmap (either id id)
  $ race (threadDelay reqTimeout >> pure (Left "Request Timeout")) $ do
    req <- loop mempty 0
    pure $
      case B.break newline req of
        (r, "\r\n") -> Right r
        (r, "\n")   -> Right r
        (_, "")     -> Left "Request too big or unterminated"
        _           -> Left "Unexpected data after newline"
  where newline = (||)
          <$> (== asciiOrd '\n')
          <*> (== asciiOrd '\r')
        reqTimeout = 10000000 -- 10s
        maxSize = 1024 * 1024
        loop bs size = do
          part <- receive sock maxSize msgNoSignal
          let newSize = size + B.length part
          if newSize >= maxSize || part == mempty || B.elem (asciiOrd '\n') part
            then pure $ bs `mappend` part
            else loop (bs `mappend` part) newSize

-- | Auxiliary function that sets up the listening socket for
--   'runGopherManual' correctly and starts to listen.
--
--   May throw a 'SocketException' if an error occurs while
--   setting up the socket.
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

-- $runGopherVariants
-- The @runGopher@ function variants will generally not throw exceptions,
-- but handle them somehow (usually by logging that a non-fatal exception
-- occurred) except if the exception occurrs in the setup step of
-- 'runGopherManual'.
--
-- You'll have to handle those exceptions yourself. To see which exceptions
-- can be thrown by 'runGopher' and 'runGopherPure', read the documentation
-- of 'setupGopherSocket'.

-- | Run a gopher application that may cause effects in 'IO'.
--   The application function is given the 'GopherRequest'
--   sent by the client and must produce a GopherResponse.
runGopher :: GopherConfig -> (GopherRequest -> IO GopherResponse) -> IO ()
runGopher cfg f = runGopherManual (setupGopherSocket cfg) (pure ()) close cfg f

-- | Same as 'runGopher', but allows you to setup the 'Socket' manually
--   and calls an user provided action soon as the server is ready
--   to accept requests. When the server terminates, it calls the given
--   clean up action which must close the socket and may perform other
--   shutdown tasks (like notifying a supervisor it is stopping).
--
--   Spacecookie assumes the 'Socket' is properly set up to listen on the
--   port and host specified in the 'GopherConfig' (i. e. 'bind' and
--   'listen' have been called). This can be achieved using 'setupGopherSocket'.
--   Especially note that spacecookie does /not/ check if the listening
--   address and port of the given socket match 'cListenAddr' and
--   'cServerPort'.
--
--   This is intended for supporting systemd socket activation and storage,
--   but may also be used to support other use cases where more control is
--   necessary. Always use 'runGopher' if possible, as it offers less ways
--   of messing things up.
runGopherManual :: IO (Socket Inet6 Stream TCP)         -- ^ action to set up listening socket
                -> IO ()                                -- ^ ready action called after startup
                -> (Socket Inet6 Stream TCP -> IO ())   -- ^ socket clean up action
                -> GopherConfig                         -- ^ server config
                -> (GopherRequest -> IO GopherResponse) -- ^ request handler
                -> IO ()
runGopherManual sockAction ready term cfg f = bracket
  sockAction
  term
  (\sock -> do
    gopherM (Env cfg f) $ do
      addr <- liftIO $ getAddress sock
      logInfo $ "Listening on " <> toGopherLogStr addr

      liftIO $ ready

      forever $ acceptAndHandle sock)

forkGopherM :: GopherM () -> IO () -> GopherM ThreadId
forkGopherM action cleanup = do
  env <- ask
  liftIO $ forkIO $ do
    gopherM env action `catch`
      (logException
        (cLogHandler $ serverConfig env)
        "Thread failed with exception: " :: SomeException -> IO ())
    cleanup

-- | Split an selector in the actual search selector and
--   an optional search expression as documented in the
--   RFC1436 appendix.
splitSelector :: ByteString -> (ByteString, Maybe ByteString)
splitSelector = second checkSearch . B.breakSubstring "\t"
  where checkSearch search =
          if B.length search > 1
            then Just $ B.tail search
            else Nothing

handleIncoming :: Socket Inet6 Stream TCP -> SocketAddress Inet6 -> GopherM ()
handleIncoming clientSock addr@(SocketAddressInet6 cIpv6 _ _ _) = do
  request <- liftIO $ receiveRequest clientSock
  logger <- cLogHandler . serverConfig <$> ask
  intermediateResponse <-
    case request of
      Left e -> pure $ ErrorResponse e
      Right rawSelector -> do
        let (onlySel, search) = splitSelector rawSelector
            req = GopherRequest
              { requestRawSelector = rawSelector
              , requestSelector = onlySel
              , requestSearchString = search
              , requestClientAddr  = inet6AddressToTuple cIpv6
              }

        logInfo $ "New Request \"" <> toGopherLogStr rawSelector <> "\" from "
          <> makeSensitive (toGopherLogStr addr)

        fun <- serverFun <$> ask
        liftIO $ fun req `catch` \e -> do
          let msg = "Unhandled exception in handler: "
                <> toGopherLogStr (show (e :: SomeException))
          logIO logger GopherLogLevelError msg
          pure $ ErrorResponse "Unknown error occurred"

  rawResponse <- response intermediateResponse

  liftIO $ void (sendAll clientSock rawResponse msgNoSignal) `catch` \e ->
    logException logger "Exception while sending response to client: " (e :: SocketException)

acceptAndHandle :: Socket Inet6 Stream TCP -> GopherM ()
acceptAndHandle sock = do
  connection <- liftIO $ fmap Right (accept sock) `catch` (pure . Left)
  case connection of
    Left e -> logError $ "Failure while accepting connection "
      <> toGopherLogStr (show (e :: SocketException))
    Right (clientSock, addr) -> do
      logInfo $ "New connection from " <> makeSensitive (toGopherLogStr addr)
      void $ forkGopherM (handleIncoming clientSock addr) (gracefulClose clientSock)

-- | Like 'runGopher', but may not cause effects in 'IO' (or anywhere else).
runGopherPure :: GopherConfig -> (GopherRequest -> GopherResponse) -> IO ()
runGopherPure cfg f = runGopher cfg (fmap pure f)

response :: GopherResponse -> GopherM ByteString
response (FileResponse str) = pure str
response (ErrorResponse reason) = response . MenuResponse $
    [ Item Error reason "Err" Nothing Nothing ]
response (MenuResponse items) =
  let appendItem cfg acc (Item fileType title path host port) =
        acc <> BB.word8 (fileTypeToChar fileType) <> mconcat
          [ BB.byteString title
          , BB.charUtf8 '\t'
          , BB.byteString path
          , BB.charUtf8 '\t'
          , BB.byteString $ fromMaybe (cServerName cfg) host
          , BB.charUtf8 '\t'
          , BB.intDec . fromIntegral $ fromMaybe (cServerPort cfg) port
          , BB.byteString "\r\n"
          ]
   in do
  cfg <- serverConfig <$> ask
  pure . BL.toStrict . BB.toLazyByteString
    $ foldl (appendItem cfg) mempty items
