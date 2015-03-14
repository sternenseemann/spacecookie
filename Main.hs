import           Prelude               hiding (lookup)

import           Control.Applicative   ((<$>), (<*>), liftA2)
import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever, unless, when)
import           Data.ByteString.Char8 (ByteString (), pack, unpack)
import qualified Data.ByteString.Char8 as B
import           Data.Char             (toLower)
import           Data.Map              (Map (..), fromList, lookup)
import           Data.Maybe            (fromJust)
import           Gopher.Types
import           Network.Socket        (Family (..), PortNumber (),
                                        SockAddr (..), Socket (..),
                                        SocketOption (..), SocketType (..),
                                        accept, bind, defaultProtocol,
                                        iNADDR_ANY, listen, sClose,
                                        setSocketOption, socket, socketToHandle)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getDirectoryContents,
                                        setCurrentDirectory)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)
import           System.FilePath       (takeExtension)
import           System.IO             (BufferMode (..), IOMode (..), hClose,
                                        hGetLine, hPutStr, hSetBuffering)
import           System.Posix.Signals  (Handler (..), installHandler,
                                        keyboardSignal)
import           System.Posix.User     (UserEntry (..), getRealUserID,
                                        getUserEntryForName, setGroupID,
                                        setUserID)
serverName :: ByteString
serverName = pack "localhost"

serverPort :: PortNumber
serverPort = 7070

runUserName :: ByteString
runUserName = pack "lukas"

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
gopherFileType :: GopherPath -> IO GopherFileType
gopherFileType f = do
  isDir <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath
  return $ cond [ (isDir, Directory)
                , (isGif, GifFile)
                , (isImage, ImageFile)
                , (isFile, File)
                , (True, Error)]
  where isGif = takeExtension filePath == "gif"
        isImage = isGif || map toLower (takeExtension filePath) `elem` ["png", "jpg", "jpeg", "raw", "cr2", "nef"]
        filePath = destructGopherPath f

stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s               = B.empty
  | B.head s `elem` "\n\r" = pack "" `B.append` stripNewline (B.tail s)
  | otherwise              = B.head s `B.cons` stripNewline (B.tail s)

requestToResponse :: GopherPath -> GopherFileType -> (FilePath -> IO GopherResponse)
requestToResponse path fileType
  | isFile fileType       = fileResponse
  | fileType == Directory = directoryResponse
  | otherwise             = \f -> return $
    ErrorResponse (pack "An error occured while handling your request") serverName serverPort

fileResponse :: FilePath -> IO GopherResponse
fileResponse fp = FileResponse <$> B.readFile fp

directoryResponse :: FilePath -> IO GopherResponse
directoryResponse fp = do
  dir <- map (combine (constructGopherPath fp)) <$> filter isListable <$> map constructGopherPath
    <$> getDirectoryContents fp
  items <- zipWith (menuItem serverName serverPort) dir <$> mapM gopherFileType dir
  return $ MenuResponse items

-- handle incoming requests
handleIncoming :: Socket -> IO ()
handleIncoming clientSock = do
  hdl <- socketToHandle clientSock ReadWriteMode
  hSetBuffering hdl NoBuffering

  line <- stripNewline `fmap` B.hGetLine hdl
  let path = gopherRequestToPath line
  gopherType <- gopherFileType path

  let buildReponse :: FilePath -> IO GopherResponse
      buildReponse = requestToResponse path gopherType

  resp <- fmap response $ buildReponse $ destructGopherPath path

  B.hPutStr hdl resp
  hClose hdl

-- main loop
mainLoop :: Socket -> IO ()
mainLoop sock = do
  _ <- forever $ do
    (clientSock, _) <- accept sock
    forkIO $ handleIncoming clientSock
  cleanup sock


-- cleanup at the end
cleanup :: Socket -> IO ()
cleanup sock = do
  sClose sock
  exitFailure

dropPrivileges :: IO ()
dropPrivileges = do
  uid <- getRealUserID
  when (uid /= 0) $ return ()

  user <- getUserEntryForName $ unpack runUserName
  setGroupID $ userGroupID user
  setUserID $ userID user

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ error "Need only the root directory to serve as argument"

  let serveRoot = head args

  serveRootExists <- doesDirectoryExist serveRoot
  unless serveRootExists $ error "The specified root directory does not exist"

  -- we need for easier path building
  setCurrentDirectory serveRoot

  sock <- socket AF_INET Stream defaultProtocol
  -- make socket immediately reusable
  setSocketOption sock ReuseAddr 1


  bind sock (SockAddrInet serverPort iNADDR_ANY)
  listen sock 5

  dropPrivileges

  -- react to Crtl-C
  _ <- installHandler keyboardSignal (Catch $ cleanup sock) Nothing

  mainLoop sock
