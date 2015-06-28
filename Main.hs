{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Prelude                hiding (lookup)

import           Control.Applicative    (Applicative (..), liftA2, (<$>), (<*>))
import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever, unless, when)
import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Control.Monad.Reader   (MonadReader (..), ReaderT (..), ask)
import           Data.ByteString.Char8  (ByteString (), pack, unpack)
import qualified Data.ByteString.Char8  as B
import           Data.Char              (toLower)
import           Data.Map               (Map (..), fromList, lookup)
import           Data.Maybe             (fromJust)
import           Gopher.Types
import           Network.Socket         (Family (..), PortNumber (),
                                         SockAddr (..), Socket (..),
                                         SocketOption (..), SocketType (..),
                                         accept, bind, defaultProtocol,
                                         iNADDR_ANY, listen, sClose,
                                         setSocketOption, socket,
                                         socketToHandle)
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         getDirectoryContents,
                                         setCurrentDirectory)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import           System.FilePath        (takeExtension)
import           System.IO              (BufferMode (..), IOMode (..), hClose,
                                         hGetLine, hPutStr, hSetBuffering)
import           System.Posix.Signals   (Handler (..), installHandler,
                                         keyboardSignal)
import           System.Posix.User      (UserEntry (..), getRealUserID,
                                         getUserEntryForName, setGroupID,
                                         setUserID)
data GopherdEnv = GopherdEnv { serverSocket :: Socket
                             , serverConfig :: Config
                             }
data Config = Config { serverName  :: ByteString
                     , serverPort  :: PortNumber
                     , runUserName :: ByteString
                     }
newtype Spacecookie a = Spacecookie
                      { runSpacecookie :: ReaderT GopherdEnv IO a }
                      deriving ( Functor, Applicative, Monad
                               , MonadIO, MonadReader GopherdEnv)

-- serverName :: ByteString
-- serverName = pack "localhost"
--
-- serverPort :: PortNumber
-- serverPort = 7070
--
-- runUserName :: ByteString
-- runUserName = pack "lukas"

-- does this file deserve to be listed?
isListable :: GopherPath -> Bool
isListable p
  | null p                 = False
  | B.head (last p) == '.' = False
  | otherwise              = True

-- LISPy conditional statement
cond :: [(Bool, a)] -> a
cond [] = error "cond: no matching condition"
cond ((condition, val) : xs) = if condition
                                 then val
                                 else cond xs

-- Note: this is a very simple version of the thing we need
-- TODO: at least support for BinaryFile should be added
gopherFileType :: GopherPath -> Spacecookie GopherFileType
gopherFileType f = do
  isDir <- liftIO $ doesDirectoryExist filePath
  isFile <- liftIO $ doesFileExist filePath
  return $ cond [ (isDir, Directory)
                , (isGif, GifFile)
                , (isImage, ImageFile)
                , (isFile, File)
                , (True, Error)]
  where isGif = takeExtension filePath == "gif"
        isImage = map toLower (takeExtension filePath) `elem` ["png", "jpg", "jpeg", "raw", "cr2", "nef"] || isGif
        filePath = destructGopherPath f

stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s               = B.empty
  | B.head s `elem` "\n\r" = stripNewline (B.tail s)
  | otherwise              = B.head s `B.cons` stripNewline (B.tail s)

requestToResponse :: GopherPath -> GopherFileType -> (FilePath -> Spacecookie GopherResponse)
requestToResponse path fileType = response
  where response
          | isFile fileType       = fileResponse
          | fileType == Directory = directoryResponse
          | otherwise             = errorResponse $ pack "An error occured while handling your request"

fileResponse :: FilePath -> Spacecookie GopherResponse
fileResponse fp = liftIO $ FileResponse <$> B.readFile fp

directoryResponse :: FilePath -> Spacecookie GopherResponse
directoryResponse fp = do
  env <- ask
  let conf = serverConfig env
      host = serverName conf
      port = serverPort conf
  dir <- liftIO $ map (combine (constructGopherPath fp)) <$> filter isListable <$> map constructGopherPath <$> getDirectoryContents fp
  items <- zipWith (menuItem host port) dir <$> mapM gopherFileType dir
  return $ MenuResponse items

errorResponse :: ByteString -> FilePath -> Spacecookie GopherResponse
errorResponse errorMsg _ = do
  env <- ask
  let conf = serverConfig env
      host = serverName conf
      port = serverPort conf
  return $ ErrorResponse errorMsg host port

-- handle incoming requests
handleIncoming :: Socket -> Spacecookie ()
handleIncoming clientSock = do
  hdl <- liftIO $ socketToHandle clientSock ReadWriteMode
  liftIO $ hSetBuffering hdl NoBuffering

  line <- liftIO $ stripNewline <$> B.hGetLine hdl
  let path = gopherRequestToPath line
  gopherType <- gopherFileType path

  let buildReponse :: FilePath -> Spacecookie GopherResponse
      buildReponse = requestToResponse path gopherType

  resp <- fmap response $ buildReponse $ destructGopherPath path

  liftIO $ B.hPutStr hdl resp
  liftIO $ hClose hdl

-- main loop
mainLoop :: Spacecookie ()
mainLoop = do
  env <- ask
  let sock = serverSocket env
  _ <- forever $ do
    (clientSock, _) <- liftIO $ accept sock
    liftIO $ forkIO $ (runReaderT . runSpacecookie) (handleIncoming clientSock) env
  cleanup


-- cleanup at the end
cleanup :: Spacecookie ()
cleanup = do
  env <- ask
  let sock = serverSocket env
  liftIO $ sClose sock
  liftIO exitFailure

dropPrivileges :: Spacecookie ()
dropPrivileges = do
  env <- ask
  let conf = serverConfig env
  uid <- liftIO getRealUserID
  when (uid /= 0) $ return ()

  user <- liftIO $ getUserEntryForName $ unpack $ runUserName conf
  liftIO $ setGroupID $ userGroupID user
  liftIO $ setUserID $ userID user

spacecookieMain :: Spacecookie ()
spacecookieMain = do
  dropPrivileges

  -- react to Crtl-C
--  _ <- installHandler keyboardSignal (Catch $ liftIO $ cleanup) Nothing
  mainLoop

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ error "Need only the root directory to serve as argument"

  let serveRoot = head args
      conf      = Config { serverName   = pack "localhost"
                         , serverPort   = 7070
                         , runUserName  = pack "lukas"
                         }

  serveRootExists <- doesDirectoryExist serveRoot
  unless serveRootExists $ error "The specified root directory does not exist"

  -- needed for easier path building
  setCurrentDirectory serveRoot

  sock <- socket AF_INET Stream defaultProtocol
  -- make socket immediately reusable
  setSocketOption sock ReuseAddr 1

  bind sock (SockAddrInet (serverPort conf) iNADDR_ANY)
  listen sock 5

  let env = GopherdEnv sock conf
  (runReaderT . runSpacecookie) spacecookieMain env
