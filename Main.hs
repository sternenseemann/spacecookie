import           Control.Concurrent   (forkIO)
import           Control.Monad        (forever, unless, when)
import           Data.Maybe           (fromJust)
import           Network.Socket
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getDirectoryContents,
                                       setCurrentDirectory)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.FilePath      (combine, takeFileName, (</>))
import           System.IO            (BufferMode (..), IOMode (..), hClose,
                                       hGetLine, hPutStr, hSetBuffering)
import           System.Posix.Signals (Handler (..), installHandler,
                                       keyboardSignal)
import           System.Posix.User    (UserEntry (..), getRealUserID,
                                       getUserEntryForName, setGroupID,
                                       setUserID)

serverName :: String
serverName = "localhost"

serverPort :: PortNumber
serverPort = 70

runUserName :: String
runUserName = "lukas"

data GopherFileType = File
  | Directory
  | PhoneBookServer
  | Error
  | BinHexMacintoshFile
  | DOSArchive
  | UnixUuencodedFile
  | IndexSearchServer
  | TelnetSession
  | BinaryFile
  | RedundantServer
  | Tn3270Session
  | GifFile
  | ImageFile
  deriving (Show, Eq)

fileTypeToCharRelation :: [(GopherFileType, Char)]
fileTypeToCharRelation =
  [ (File, '0')
  , (Directory, '1')
  , (PhoneBookServer, '2')
  , (Error, '3')
  , (BinHexMacintoshFile, '4')
  , (DOSArchive, '5')
  , (UnixUuencodedFile, '6')
  , (IndexSearchServer, '7')
  , (TelnetSession, '8')
  , (BinaryFile, '9')
  , (RedundantServer, '+')
  , (Tn3270Session, 'T')
  , (GifFile, 'g')
  , (ImageFile, 'I') ]

fileTypeToChar :: GopherFileType -> Char
fileTypeToChar t = fromJust $ lookup t fileTypeToCharRelation

-- Note: this is a very simple version of the thing we need
-- TODO: at least support for GifFile ImageFile and BinaryFile should be added
-- (the other FileTypes are mostly there for the sake of completeness)
gopherFileType :: FilePath -> IO GopherFileType
gopherFileType f = do
  isDir <- doesDirectoryExist f
  isFile <- doesFileExist f
  return $ case (isDir, isFile) of
       (True, False) -> Directory
       (False, True) -> File
       _             -> Error

stripNewline :: String -> String
stripNewline "" = ""
stripNewline (x:xs)
  | x `elem` "\n\r" = "" ++ stripNewline xs
  | otherwise       = x   : stripNewline xs

checkPath :: FilePath -> String -> IO (FilePath, GopherFileType)
checkPath root line = if line == ""
                         then return (root, Directory)
                         else do
                                let path = root </> (if head line /= '/' then line else tail line)
                                fileType <- gopherFileType path
                                return (path, fileType)


gopherDirectoryEntry :: GopherFileType -> String -> FilePath -> String
gopherDirectoryEntry fileType title path = fileTypeToChar fileType : title ++ "\t" ++
                                                                     path ++ "\t" ++
                                                                     serverName ++ "\t" ++
                                                                     show serverPort ++ "\r\n"

fileResponse :: FilePath -> IO String
fileResponse = readFile

-- Response for a requested Directory
directoryEntry :: (FilePath, GopherFileType) -> String
directoryEntry (fp, ft) = if head (takeFileName fp) /= '.'
                                   then gopherDirectoryEntry ft (takeFileName fp) fp
                                   else ""

buildDirectoryResponse :: [(FilePath, GopherFileType)] -> String
buildDirectoryResponse = foldl (\acc f -> acc ++ directoryEntry f) ""

directoryResponse :: FilePath -> IO String
directoryResponse path = do
  directory <- map (combine path) `fmap` getDirectoryContents path
  types <- mapM gopherFileType directory
  let filesWithTypes = zip (map tail directory) types
  return $ buildDirectoryResponse filesWithTypes

-- Response for a error
errorResponse :: FilePath -> IO String
errorResponse fp = return $ fileTypeToChar Error : "Error opening: '" ++ fp ++ "'\tErr\t" ++ serverName ++ "\t" ++ show serverPort

-- handle incoming requests
handleIncoming :: Socket -> String -> IO ()
handleIncoming clientSock root = do
  hdl <- socketToHandle clientSock ReadWriteMode
  hSetBuffering hdl NoBuffering

  line <- stripNewline `fmap` hGetLine hdl
  (path, fileType) <- checkPath root line

  response <- case fileType of
      Directory -> directoryResponse path
      File      -> fileResponse path
      _         -> errorResponse path

  hPutStr hdl response
  hClose hdl

-- main loop
mainLoop :: Socket -> FilePath -> IO ()
mainLoop sock root = forever $ do
  (clientSock, _) <- accept sock
  forkIO $ handleIncoming clientSock root


-- cleanup at the end
cleanup :: Socket -> IO ()
cleanup sock = do
  sClose sock
  exitFailure

dropPrivileges :: IO ()
dropPrivileges = do
  uid <- getRealUserID
  when (uid /= 0) $ return ()

  user <- getUserEntryForName runUserName
  setGroupID $ userGroupID user
  setUserID $ userID user

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ error "Need only the root directory to serve as argument"

  let root = head args

  rootExists <- doesDirectoryExist root
  unless rootExists $ error "The specified root directory does not exist"

  -- we need for easier path building
  setCurrentDirectory root

  sock <- socket AF_INET Stream defaultProtocol
  -- make socket immediately reusable
  setSocketOption sock ReuseAddr 1


  bind sock (SockAddrInet serverPort iNADDR_ANY)
  listen sock 5

  dropPrivileges

  -- react to Crtl-C
  _ <- installHandler keyboardSignal (Catch $ cleanup sock) Nothing

  mainLoop sock root
