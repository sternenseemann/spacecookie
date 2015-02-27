import           Control.Monad        (forever, unless, when)
import           Data.Maybe           (fromJust)
import           Network.Socket
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getDirectoryContents,
                                       setCurrentDirectory)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)
import           System.FilePath      (combine, takeFileName, (</>))
import           System.Posix.Signals (Handler (..), installHandler,
                                       keyboardSignal)

serverName :: String
serverName = "localhost"

serverPort :: String
serverPort = "7070"

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


receiveLine :: Socket -> IO String
receiveLine sock = receiveUntilLineBreak ""
  where receiveUntilLineBreak :: String -> IO String
        receiveUntilLineBreak acc
          | noFailLast acc == '\n' = return acc
          | otherwise = do
            chunk <- recv sock 256
            receiveUntilLineBreak $ acc ++ chunk
          where noFailLast "" = '\0'
                noFailLast x = last x

stripNewline :: String -> String
stripNewline "" = ""
stripNewline (x:xs)
  | x `elem` "\n\r" = "" ++ stripNewline xs
  | otherwise       = x   : stripNewline xs

receiveGopherLine :: Socket -> IO String
receiveGopherLine sock = stripNewline `fmap` receiveLine sock

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
                                                                     serverPort ++ "\r\n"

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
errorResponse fp = return $ fileTypeToChar Error : "Error opening: '" ++ fp ++ "'\tErr\t" ++ serverName ++ "\t" ++ serverPort

-- handle incoming requests
handleIncoming :: Socket -> String -> IO ()
handleIncoming sock root = do
  (clientSock, _) <- accept sock
  line <- stripNewline `fmap` receiveGopherLine clientSock
  (path, fileType) <- checkPath root line

  response <- case fileType of
      Directory -> directoryResponse path
      File      -> fileResponse path
      _         -> errorResponse path

  sentBytes <- send clientSock response
  when (sentBytes /= length response) (putStrLn "Warning: Not all bytes were sent")
  close clientSock

-- cleanup at the end
cleanup :: Socket -> IO ()
cleanup sock = do
  close sock
  exitFailure

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
  bind sock (SockAddrInet 7070 iNADDR_ANY)
  listen sock 5

  -- react to Crtl-C
  _ <- installHandler keyboardSignal (Catch $ cleanup sock) Nothing

  forever $ handleIncoming sock root
