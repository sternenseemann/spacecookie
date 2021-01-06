{-# LANGUAGE OverloadedStrings #-}
import Config
import Systemd

import Paths_spacecookie (version)

import Network.Gopher
import Network.Gopher.Util (sanitizePath, uEncode)
import Network.Gopher.Util.Gophermap
import qualified Data.ByteString as B
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))
import Control.Monad (when, unless, filterM, join)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString.Builder (Builder ())
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe)
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment
import System.FilePath.Posix (takeFileName, takeExtension, (</>), dropDrive, splitDirectories)
import qualified System.Log.FastLogger as FL
import System.Posix.Directory (changeWorkingDirectory)
import System.Exit

data Flags = Version | Usage

options :: [OptDescr Flags]
options =
  [ Option "h" [ "help", "usage" ] (NoArg Usage)   "Print usage information"
  , Option []  [ "version" ]       (NoArg Version) "Show used version of spacecookie"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    ([], [configFile], []) -> runServer configFile
    -- this works because we only have two flags atm
    ([Version], _, []) -> putStrLn $ showVersion version
    (_, _, []) -> printUsage
    (_, _, es) -> die . mconcat $
      "errors occurred while parsing options:\n":es

runServer :: FilePath -> IO ()
runServer configFile = do
  doesFileExist configFile >>=
    (flip unless) (die "could not open config file")
  config' <- eitherDecodeFileStrict' configFile
  case config' of
    Right config -> do
      changeWorkingDirectory (rootDirectory config)
      (logHandler, logStopAction) <- fromMaybe (Nothing, pure ())
        . fmap (first Just) <$> makeLogHandler (logConfig config)
      let cfg = GopherConfig
            { cServerName = serverName config
            , cListenAddr = listenAddr config
            , cServerPort = serverPort config
            , cRunUserName = runUserName config
            , cLogHandler = logHandler
            }
      runGopherManual
        (systemdSocket cfg)
        (notifyReady >> pure ())
        (\s -> do
          _ <- notifyStopping
          logStopAction
          systemdStoreOrClose s)
        cfg spacecookie
    Left err -> die $ "failed to parse config: " ++ err

printUsage :: IO ()
printUsage = do
  n <- getProgName
  putStrLn . flip usageInfo options $
    mconcat [ "Usage: ", n, " CONFIG\n" ]

makeLogHandler :: LogConfig -> IO (Maybe (GopherLogHandler, IO ()))
makeLogHandler lc =
  let wrapTimedLogger :: FL.TimedFastLogger -> FL.FastLogger
      wrapTimedLogger logger str = logger $ (\t ->
        "[" <> FL.toLogStr t <> "]" <> str)
      formatLevel lvl =
        case lvl of
          GopherLogLevelInfo  -> "[info] "
          GopherLogLevelError -> "[err ] "
      processMsg =
        if logHideIps lc
          then hideSensitive
          else id
      logHandler :: FL.FastLogger -> GopherLogLevel -> GopherLogStr -> IO ()
      logHandler logger lvl msg = when (lvl <= logLevel lc) . logger
        $  formatLevel lvl
        <> ((FL.toLogStr :: Builder -> FL.LogStr) . fromGopherLogStr . processMsg $ msg)
        <> "\n"
      logType = FL.LogStderr FL.defaultBufSize
   in sequenceA . (flip boolToMaybe) (logEnable lc) $ do
     (logger, cleanup) <-
       if logHideTime lc
         then FL.newFastLogger logType
         else first wrapTimedLogger <$> do
           timeCache <- FL.newTimeCache FL.simpleTimeFormat
           FL.newTimedFastLogger timeCache logType
     pure (logHandler logger, cleanup)

spacecookie :: String -> IO GopherResponse
spacecookie path' = do
  let path = "." </> dropDrive (sanitizePath path')
  ft <- gopherFileType path
  pt <- pathType path

  if not (isListable pt path') && pt /= DoesNotExist
    then pure . ErrorResponse $ "Accessing '" ++ path' ++ "' is not allowed."
    else case ft of
           Error -> pure $
             if "URL:" `isPrefixOf` path'
               then ErrorResponse $ mconcat
                 [ "spacecookie does not support proxying HTTP, "
                 , "try using a gopher client that supports URL: selectors. "
                 , "If you tried to request a file called '"
                 , path', "', it does not exist." ]
               else ErrorResponse $ "The requested file '" ++ path' ++ "' does not exist or is not available."
           -- always use gophermapResponse which falls back
           -- to directoryResponse if there is no gophermap file
           Directory -> gophermapResponse path
           _ -> fileResponse path

fileResponse :: FilePath -> IO GopherResponse
fileResponse path = FileResponse <$> B.readFile path

makeAbsolute :: FilePath -> FilePath
makeAbsolute x =
  case x of
    ('.':'/':_) -> tail x
    "." -> "/"
    _ -> x

directoryResponse :: FilePath -> IO GopherResponse
directoryResponse path = do
  dir <- join (filterM (\x -> ((flip isListable) x) <$> pathType x) . map (path </>) <$> getDirectoryContents path)
  fileTypes <- mapM gopherFileType dir
  pure . MenuResponse . map (\f -> f Nothing Nothing) $ zipWith (\t f -> Item t (uEncode (takeFileName f)) f) fileTypes (map makeAbsolute dir)

gophermapResponse :: FilePath -> IO GopherResponse
gophermapResponse path = do
  let gophermap = path </> ".gophermap"
  exists <- doesFileExist gophermap
  parsed <- if exists
              then parseOnly parseGophermap <$> B.readFile gophermap
              else pure $ Left "Gophermap file does not exist"
  case parsed of
    Left _ -> directoryResponse path
    Right right -> pure $ gophermapToDirectoryResponse (makeAbsolute path) right

-- | calculates the file type identifier used in the Gopher protocol
-- for a given file
gopherFileType :: FilePath -> IO GopherFileType
gopherFileType f = do
  isDir  <- ioCheck Directory doesDirectoryExist
  isFile <- ioCheck File doesFileExist
  let isGif = boolToMaybe GifFile $ takeExtension f == "gif"
  let isImage = boolToMaybe ImageFile $ map toLower (takeExtension f) `elem` ["png", "jpg", "jpeg", "raw", "cr2", "nef"]
  return . fromJust $
    isDir <|> isGif <|> isImage <|>  isFile <|> Just Error
  where ioCheck onSuccess check = fmap (boolToMaybe onSuccess) . check $ f

-- | isListable filters out system files for directory listings
isListable :: PathType -> FilePath -> Bool
isListable Directory' "" = True -- "" is root
isListable _ "" = False
isListable DoesNotExist _ = False
isListable Directory' p
  | (head . last . splitDirectories) p == '.' = False
  | otherwise = True
isListable File' p
  | head (takeFileName p) == '.' = False
  | otherwise = True

-- | True -> Just a
--   False -> Nothing
boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe a True  = Just a
boolToMaybe _ False = Nothing

data PathType
  = Directory'
  | File'
  | DoesNotExist
  deriving (Show, Eq)

pathType :: FilePath -> IO PathType
pathType p = do
  file <- doesFileExist p
  dir  <- doesDirectoryExist p
  if file
    then pure File'
    else if dir
      then pure Directory'
      else pure DoesNotExist
