{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude              hiding (lookup)
import           Prelude                   ()

import           Control.Concurrent        (forkIO)
import           Data.ByteString.Char8     (pack, unpack)
import qualified Data.ByteString.Char8     as B
import           Data.Map                  (fromList, lookup)
import           Data.Maybe                (fromJust)
import qualified Filesystem.Path.CurrentOS as F
import           Gopher.Types
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
import           System.Exit               (exitFailure)
import           System.IO                 (BufferMode (..), IOMode (..),
                                            hClose, hGetLine, hPutStr,
                                            hSetBuffering)
import           System.Posix.Signals      (Handler (..), installHandler,
                                            keyboardSignal)
import           System.Posix.User         (UserEntry (..), getRealUserID,
                                            getUserEntryForName, setGroupID,
                                            setUserID)

serverName :: ByteString
serverName = pack "localhost"

serverPort :: PortNumber
serverPort = 7070

runUserName :: ByteString
runUserName = pack "lukas"

-- Note: this is a very simple version of the thing we need
-- TODO: at least support for GifFile ImageFile and BinaryFile should be added
-- (the other FileTypes are mostly there for the sake of completeness)
gopherFileType :: FilePath -> GopherPath -> IO GopherFileType
gopherFileType serveRoot f = do
  isDir <- doesDirectoryExist $ F.encodeString filePath
  isFile <- doesFileExist $ F.encodeString filePath
  return $ case (isDir, isFile, isGif, isImage) of
       (True, False, False, False) -> Directory
       (False, True, False, False) -> File
       (False, True, True, True)   -> GifFile
       (False, True, False, True)  -> ImageFile
       _             -> Error
  where isGif = hasExtension filePath "gif"
        isImage = isGif || hasExtension filePath "png" || hasExtension filePath "jpg" || hasExtension filePath "jpeg" || hasExtension filePath "raw"
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

  let serveRoot = F.fromText $ head args

  serveRootExists <- doesDirectoryExist $ F.encodeString serveRoot
  unless serveRootExists $ error "The specified root directory does not exist"

  -- we need for easier path building
  setCurrentDirectory $ F.encodeString serveRoot

  sock <- socket AF_INET Stream defaultProtocol
  -- make socket immediately reusable
  setSocketOption sock ReuseAddr 1


  bind sock (SockAddrInet serverPort iNADDR_ANY)
  listen sock 5

  dropPrivileges

  -- react to Crtl-C
  _ <- installHandler keyboardSignal (Catch $ cleanup sock) Nothing

  mainLoop sock serveRoot
