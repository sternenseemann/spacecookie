{-# LANGUAGE OverloadedStrings #-}
import           Spacecookie.ConfigParsing
import           Spacecookie.Monad
import           Spacecookie.Types

import           Prelude                   hiding (lookup)

import           Control.Applicative       (Applicative (..), liftA2, (<$>),
                                            (<*>))
import           Control.Concurrent        (forkIO)
import           Control.Monad             (forever, mzero, unless, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ask, runReaderT)
import           Data.ByteString.Char8     (ByteString (), pack, unpack)
import qualified Data.ByteString.Char8     as B
import           Data.Char                 (toLower)
import           Data.Map                  (Map (..), fromList, lookup)
import           Data.Maybe                (fromJust, isNothing)
import           Data.Text                 (Text ())
import           Data.Yaml                 (decode)
import           Network.Socket            (Family (..), PortNumber (),
                                            SockAddr (..), Socket (..),
                                            SocketOption (..), SocketType (..),
                                            accept, bind, defaultProtocol,
                                            iNADDR_ANY, listen, sClose,
                                            setSocketOption, socket,
                                            socketToHandle)
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            getDirectoryContents,
                                            setCurrentDirectory)
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure)
import           System.FilePath           (takeExtension)
import           System.IO                 (BufferMode (..), IOMode (..),
                                            hClose, hGetLine, hPutStr,
                                            hSetBuffering)
import           System.Posix.Signals      (Handler (..), installHandler,
                                            keyboardSignal)
import           System.Posix.User         (UserEntry (..), getRealUserID,
                                            getUserEntryForName, setGroupID,
                                            setUserID)


-- | isListable filters out system files for directory listnings
isListable :: GopherPath -> Bool
isListable p
  | null p                 = False
  | B.head (last p) == '.' = False
  | otherwise              = True

-- | cond is a LISPy conditional statement
cond :: [(Bool, a)] -> a
cond [] = error "cond: no matching condition"
cond ((condition, val) : xs) = if condition
                                 then val
                                 else cond xs

-- TODO: at least support for BinaryFile should be added
-- | calculates the file type identifier used in the Gopher protocol
-- for a given file
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

-- | strips "\n" and "\r" from a string. Used on all strings
-- coming from the client to make them parseable.
stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s           = B.empty
  | B.head s `elem`
    ("\n\r" :: String) = stripNewline (B.tail s)
  | otherwise          = B.head s `B.cons` stripNewline (B.tail s)

-- | requestToResponse takes a Path and a file type and returns the function
-- that calculates the response for a given file.
requestToResponse :: GopherPath -> GopherFileType -> FilePath -> Spacecookie GopherResponse
requestToResponse path fileType = response
  where response
          | isFile fileType       = fileResponse
          | fileType == Directory = directoryResponse
          | otherwise             = errorResponse $ pack "An error occured while handling your request"

-- | creates a gopher file response
fileResponse :: FilePath -> Spacecookie GopherResponse
fileResponse fp = liftIO $ FileResponse <$> B.readFile fp

-- | creates a gopher directory response
directoryResponse :: FilePath -> Spacecookie GopherResponse
directoryResponse fp = do
  env <- ask
  let conf = serverConfig env
      host = serverName conf
      port = serverPort conf
  dir <- liftIO $ map (combine (constructGopherPath fp)) <$> filter isListable <$> map constructGopherPath <$> getDirectoryContents fp
  items <- zipWith (menuItem host port) dir <$> mapM gopherFileType dir
  return $ MenuResponse items

-- | creates a gopher error response
errorResponse :: ByteString -> FilePath -> Spacecookie GopherResponse
errorResponse errorMsg _ = do
  env <- ask
  let conf = serverConfig env
      host = serverName conf
      port = serverPort conf
  return $ ErrorResponse errorMsg host port

-- | handleIncoming is used to handle a client (socket).
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

-- | main loop
mainLoop :: Spacecookie ()
mainLoop = do
  env <- ask
  let sock = serverSocket env
  forever $ do
    (clientSock, _) <- liftIO $ accept sock
    liftIO $ forkIO $ (runReaderT . runSpacecookie) (handleIncoming clientSock) env
  liftIO $ cleanup sock


-- | cleanup closes the socket
cleanup :: Socket -> IO ()
cleanup sock = do
  sClose sock
  exitFailure

-- | dropPrivileges is used to run spacecookie as
-- a normal user after the socket has been setup
-- (as root).
dropPrivileges :: Spacecookie ()
dropPrivileges = do
  env <- ask
  let conf = serverConfig env
  uid <- liftIO getRealUserID
  when (uid /= 0) $ return ()

  user <- liftIO $ getUserEntryForName $ unpack $ runUserName conf
  liftIO $ setGroupID $ userGroupID user
  liftIO $ setUserID $ userID user

-- | does the setup in the Spacecookie monad
-- and starts the main loop.
spacecookieMain :: Spacecookie ()
spacecookieMain = do
  dropPrivileges
  env <- ask

  -- react to Crtl-C
  liftIO $ installHandler keyboardSignal (Catch $ cleanup $ serverSocket env) Nothing
  mainLoop

-- | parses args and config and binds the socket
main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ error "Usage: spacecookie <configfile>"

  let configFile = head args

  decodedConf <- decode <$> B.readFile configFile
  when (isNothing decodedConf) $ error "Could not parse the configuration"
  let conf      = fromJust decodedConf
      serveRoot = rootDirectory conf


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
