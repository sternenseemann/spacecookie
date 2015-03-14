import           Prelude               hiding (lookup)

import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever, unless, when)
import           Data.ByteString.Char8 (ByteString (), pack, unpack)
import qualified Data.ByteString.Char8 as B
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

-- LISPy conditional statement
cond :: [(Bool, a)] -> a
cond [] = error "cond: no matching condition"
cond ((condition, val) : xs) = if condition
                                 then val
                                 else cond xs

-- Note: this is a very simple version of the thing we need
-- TODO: at least support for BinaryFile should be added
gopherFileType :: FilePath -> GopherPath -> IO GopherFileType
gopherFileType serveRoot f = do
  isDir <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath
  return $ cond [ (isDir, Directory)
                , (isGif, GifFile)
                , (isImage, ImageFile)
                , (isFile, File)
                , (True, Error)]
  where isGif = takeExtension filePath == "gif"
        isImage = isGif || takeExtension filePath `elem` ["png", "jpg", "jpeg", "raw", "cr2", "nef"]
        filePath = destructGopherPath serveRoot f

stripNewline :: ByteString -> ByteString
stripNewline s
  | B.null s               = B.empty
  | B.head s `elem` "\n\r" = pack "" `B.append` stripNewline (B.tail s)
  | otherwise              = B.head s `B.cons` stripNewline (B.tail s)

-- handle incoming requests
handleIncoming :: Socket -> FilePath -> IO ()
handleIncoming clientSock serveRoot = do
  hdl <- socketToHandle clientSock ReadWriteMode
  hSetBuffering hdl NoBuffering

  line <- stripNewline `fmap` B.hGetLine hdl
  let path = gopherRequestToPath line
  gopherType <- gopherFileType serveRoot path

  --hPutStr hdl $ unpack response
  hClose hdl

-- main loop
mainLoop :: Socket -> FilePath -> IO ()
mainLoop sock serveRoot = do
  _ <- forever $ do
    (clientSock, _) <- accept sock
    forkIO $ handleIncoming clientSock serveRoot
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

  mainLoop sock serveRoot
