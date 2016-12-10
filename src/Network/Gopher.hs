{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Gopher
  ( runGopher
  , runGopherPure
  , gophermapToDirectoryResponse
  , GopherConfig (..)
  , GopherResponse (..)
  , Gophermap (..)
  , GophermapEntry (..)
  , GopherFileType (..)
  , GopherMenuItem (..)
  ) where

import Network.Gopher.Types
import Network.Gopher.Util
import Network.Gopher.Util.Gophermap

import Network.Socket
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import Data.Maybe (isJust, fromJust, fromMaybe)
import Control.Applicative ((<$>), (<*>), Applicative (..))
import Control.Concurrent (forkIO)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad.Reader (ask, runReaderT, MonadReader (..), ReaderT (..))
import qualified Data.String.UTF8 as U
import System.IO
import System.Posix.User

-- | necessary information to handle gopher requests
data GopherConfig
  = GopherConfig { cServerName    :: ByteString   -- ^ “name” of the server (either ip address or dns name)
                 , cServerPort    :: PortNumber   -- ^ port to listen on
                 , cRunUserName   :: Maybe String -- ^ user to run the process as
                 }

data Env
  = Env { serverSocket :: Socket
        , serverName   :: ByteString
        , serverPort   :: PortNumber
        , serverFun    :: (String -> IO GopherResponse)
        }

newtype GopherM a = GopherM { runGopherM :: ReaderT Env IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Env)

-- | handleIncoming is used to handle a client (socket).
handleIncoming :: Socket -> GopherM ()
handleIncoming clientSock = do
  hdl <- liftIO $ socketToHandle clientSock ReadWriteMode
  liftIO $ hSetBuffering hdl NoBuffering

  req <- liftIO $ uDecode . stripNewline <$> B.hGetLine hdl

  fun <- serverFun <$> ask
  res <- liftIO (fun req) >>= response

  liftIO $ B.hPutStr hdl res
  liftIO $ hClose hdl

dropPrivileges :: String -> IO ()
dropPrivileges username = do
  uid <- getRealUserID
  when (uid /= 0) $ return ()

  user <- getUserEntryForName username
  setGroupID $ userGroupID user
  setUserID $ userID user

-- | Run a gopher application that may cause effects in IO.
--   The application function is given the gopher request (path)
--   and required to produce a GopherResponse.
runGopher :: GopherConfig -> (String -> IO GopherResponse) -> IO ()
runGopher cfg f = do
  -- setup the socket
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (cServerPort cfg) iNADDR_ANY)
  listen sock 5

  -- Change UID and GID if necessary
  when (isJust (cRunUserName cfg)) $ dropPrivileges (fromJust (cRunUserName cfg))

  (flip (runReaderT . runGopherM)) (Env sock (cServerName cfg) (cServerPort cfg) f) $
    forever $ do
      env <- ask
      let sock = serverSocket env
      (clientSock, _) <- liftIO $ accept sock
      liftIO . forkIO
        $ (runReaderT . runGopherM) (handleIncoming clientSock) env

-- | Run a gopher application that may not cause effects in IO.
runGopherPure :: GopherConfig -> (String -> GopherResponse) -> IO ()
runGopherPure cfg f = runGopher cfg (\x -> pure (f x))

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
    B.concat [reason, uEncode $  "\tErr\t", serverName env, uEncode "\t", uEncode . show $ serverPort env, uEncode "\r\n"]
