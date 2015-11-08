{-# LANGUAGE OverloadedStrings #-}

import           Spacecookie.Gophermap
import           Spacecookie.Monad
import           Spacecookie.Types
import           Spacecookie.ConfigParsing

import           Prelude                          hiding (lookup)

import           Control.Applicative              (Applicative (..), liftA2,
                                                   (<$>), (<*>), (<|>))
import           Control.Concurrent               (forkIO)
import           Control.Monad                    (forever, mzero, unless, when)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (ask, runReaderT)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            (ByteString (), pack, unpack)
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (toLower)
import           Data.Either                      (isLeft)
import           Data.Map                         (Map (..), fromList, lookup)
import           Data.Maybe                       (fromJust, isNothing)
import           Data.Text                        (Text ())
import           Data.Yaml                        (decode)
import           Network.Socket                   (Family (..), PortNumber (),
                                                   SockAddr (..), Socket (..),
                                                   SocketOption (..),
                                                   SocketType (..), accept,
                                                   bind, defaultProtocol,
                                                   iNADDR_ANY, listen, sClose,
                                                   setSocketOption, socket,
                                                   socketToHandle)
import           System.Directory                 (doesDirectoryExist,
                                                   doesFileExist,
                                                   getDirectoryContents,
                                                   setCurrentDirectory)
import           System.Environment               (getArgs)
import           System.Exit                      (exitFailure)
import           System.FilePath                  (takeExtension, (</>))
import           System.IO                        (BufferMode (..), IOMode (..),
                                                   hClose, hGetLine, hPutStr,
                                                   hSetBuffering)
import           System.Posix.Signals             (Handler (..), installHandler,
                                                   keyboardSignal)
import           System.Posix.User                (UserEntry (..),
                                                   getRealUserID,
                                                   getUserEntryForName,
                                                   setGroupID, setUserID)


-- | isListable filters out system files for directory listings
isListable :: GopherPath -> Bool
isListable p
  | null p                 = False
  | B.head (last p) == '.' = False
  | otherwise              = True

-- | True -> Just a
-- False -> Nothing
boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a True  = Just a
boolToMaybe _ False = Nothing

-- TODO: at least support for BinaryFile should be added
-- (This is going to be difficult and involve a lot of black magic)
-- | calculates the file type identifier used in the Gopher protocol
-- for a given file
gopherFileType :: GopherPath -> Spacecookie GopherFileType
gopherFileType f = do
  isDir  <- ioCheck Directory doesDirectoryExist
  isFile <- ioCheck File doesFileExist
  return . fromJust $ isDir <|> isGif <|> isImage <|>  isFile <|> Just Error
  where ioCheck onSuccess check = fmap (boolToMaybe onSuccess) . liftIO
          . check $ destructGopherPath f
        isGif = boolToMaybe GifFile $ takeExtension filePath == "gif"
        isImage = boolToMaybe ImageFile $
          map toLower (takeExtension filePath) `elem`
            ["png", "jpg", "jpeg", "raw", "cr2", "nef"]
        filePath = destructGopherPath f

-- | strips "\n" and "\r" from a string. Used on all strings
-- coming from the client to make them usable.
stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s           = B.empty
  | B.head s `elem`
    ("\n\r" :: String) = stripNewline (B.tail s)
  | otherwise          = B.head s `B.cons` stripNewline (B.tail s)

-- | requestToResponse takes a Path and a file type and wether the directory has
-- a gophermap and returns the function that calculates the response for a given
-- request.
requestToResponse :: GopherPath -> GopherFileType -> Bool -> Spacecookie GopherResponse
requestToResponse path fileType hasGophermap = response
  where response
          | isFile fileType       = fileResponse filePath
          | fileType == Directory &&
            hasGophermap          = gophermapDirectoryResponse filePath
          | fileType == Directory = directoryResponse filePath
          | otherwise             = errorResponse
              "An error occured while handling your request" filePath
        filePath = destructGopherPath path

-- | Creates a gopher file response.
fileResponse :: FilePath -> Spacecookie GopherResponse
fileResponse fp = liftIO $ FileResponse <$> B.readFile fp

-- | creates a gopher directory response
directoryResponse :: FilePath -> Spacecookie GopherResponse
directoryResponse fp = do
  env <- ask
  let conf = serverConfig env
      host = serverName conf
      port = serverPort conf
  dir <- liftIO $ map ((constructGopherPath fp) ++) <$> filter isListable <$>
    map constructGopherPath <$> getDirectoryContents fp
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

-- | converts a GophermapEntry to a GopherMenuItem and adds the missing
-- information
gophermapEntryToMenuItem :: GophermapEntry -> Spacecookie GopherMenuItem
gophermapEntryToMenuItem (GophermapEntry ft desc path host port) = do
  env <- ask
  let conf = serverConfig env
      confHost = serverName conf
      confPort = serverPort conf
  return $ Item ft desc (replaceIfNothing path (gopherRequestToPath desc))
    (replaceIfNothing host confHost) (replaceIfNothing port confPort)
  where replaceIfNothing Nothing  r = r
        replaceIfNothing (Just x) _ = x

-- | gophermapToDirectoryResponse adds the missing values of the
-- parsedGophermap so that it can used to construct a response for
-- the client
gophermapToDirectoryResponse :: Gophermap -> Spacecookie GopherResponse
gophermapToDirectoryResponse entries = do
  menuItems <- mapM gophermapEntryToMenuItem entries
  return $ MenuResponse menuItems

-- | gophermapDirectoryResponse generates a DirectoryResponse from
-- the directory's .gophermap file
gophermapDirectoryResponse :: FilePath -> Spacecookie GopherResponse
gophermapDirectoryResponse fp = do
  let gophermap = fp </> ".gophermap"
  parsedGM <- liftIO $ parseOnly parseGophermap <$> B.readFile gophermap
  when (isLeft parsedGM) $ error $ "Could not parse Gophermap file " ++ gophermap
  let (Right right) = parsedGM
  gophermapToDirectoryResponse right

-- | hasGophermap determines if a directory has a corresponding
-- .gophermap file which describes the gopher menu that will be
-- sent to the client
hasGophermap :: GopherPath -> IO Bool
hasGophermap gopherPath = do
  let path = destructGopherPath gopherPath
  isRealDir <- doesDirectoryExist path
  hasGmap <- doesFileExist (path </> ".gophermap")
  return $ hasGmap && isRealDir

-- | handleIncoming is used to handle a client (socket).
handleIncoming :: Socket -> Spacecookie ()
handleIncoming clientSock = do
  hdl <- liftIO $ socketToHandle clientSock ReadWriteMode
  liftIO $ hSetBuffering hdl NoBuffering

  line <- liftIO $ stripNewline <$> B.hGetLine hdl
  let path = gopherRequestToPath line
  gopherType <- gopherFileType path
  hasGmap <- liftIO $ hasGophermap path

  resp <- fmap response
    $ requestToResponse path gopherType hasGmap

  liftIO $ B.hPutStr hdl resp
  liftIO $ hClose hdl

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
  liftIO
    $ installHandler keyboardSignal (Catch $ cleanup $ serverSocket env) Nothing

  let sock = serverSocket env
  forever $ do
    (clientSock, _) <- liftIO $ accept sock
    liftIO $ forkIO
      $ (runReaderT . runSpacecookie) (handleIncoming clientSock) env
  liftIO $ cleanup sock

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
