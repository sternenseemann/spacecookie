{-# LANGUAGE OverloadedStrings #-}
import Network.Spacecookie.Config
import Network.Spacecookie.FileType
import Network.Spacecookie.Systemd

import Paths_spacecookie (version)

import Network.Gopher
import Network.Gopher.Util (sanitizePath, uEncode, uDecode)
import Network.Gopher.Util.Gophermap
import qualified Data.ByteString as B
import Data.List (isPrefixOf)
import Control.Exception (catches, Handler (..))
import Control.Monad (when, unless)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString.Builder (Builder ())
import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Directory (doesFileExist, getDirectoryContents)
import System.Environment
import System.Exit
import System.FilePath.Posix (takeFileName, (</>), dropDrive)
import qualified System.Log.FastLogger as FL
import System.Posix.Directory (changeWorkingDirectory)
import System.Socket (SocketException ())

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
    Left err -> die $ "failed to parse config: " ++ err
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

      let setupFailureHandler e = do
            (fromMaybe noLog logHandler) GopherLogLevelError
              $  "Exception occurred in setup step: "
              <> toGopherLogStr (show e)
            logStopAction
            exitFailure
          catchSetupFailure a = a `catches`
            [ Handler (setupFailureHandler :: SystemdException -> IO ())
            , Handler (setupFailureHandler :: SocketException -> IO ())
            ]

      catchSetupFailure $ runGopherManual
        (systemdSocket cfg)
        (notifyReady >> pure ())
        (\s -> do
          _ <- notifyStopping
          logStopAction
          systemdStoreOrClose s)
        cfg $ spacecookie (fromMaybe noLog logHandler)

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
          GopherLogLevelWarn  -> "[warn] "
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
   in sequenceA . boolToMaybe (logEnable lc) $ do
     (logger, cleanup) <-
       if logHideTime lc
         then FL.newFastLogger logType
         else first wrapTimedLogger <$> do
           timeCache <- FL.newTimeCache FL.simpleTimeFormat
           FL.newTimedFastLogger timeCache logType
     pure (logHandler logger, cleanup)

noLog :: GopherLogHandler
noLog = const . const $ pure ()

spacecookie :: GopherLogHandler -> GopherRequest -> IO GopherResponse
spacecookie logger request = do
  let path' = uDecode $ requestSelector request
      path  = "." </> dropDrive (sanitizePath path')
  pt <- gopherFileType path

  case pt of
    Left PathIsNotAllowed ->
      pure . ErrorResponse $ "Accessing '" ++ path' ++ "' is not allowed."
    Left PathDoesNotExist -> pure $
      if "URL:" `isPrefixOf` path'
        then ErrorResponse $ mconcat
          [ "spacecookie does not support proxying HTTP, "
          , "try using a gopher client that supports URL: selectors. "
          , "If you tried to request a file called '"
          , path', "', it does not exist." ]
        else ErrorResponse $ "The requested file '" ++ path'
          ++ "' does not exist or is not available."
    Right ft ->
      case ft of
        Error -> pure $ ErrorResponse $ "An unknown error occurred"
        -- always use gophermapResponse which falls back
        -- to directoryResponse if there is no gophermap file
        Directory -> gophermapResponse logger path
        _ -> fileResponse logger path

fileResponse :: GopherLogHandler -> FilePath -> IO GopherResponse
fileResponse _ path = FileResponse <$> B.readFile path

makeAbsolute :: FilePath -> FilePath
makeAbsolute x =
  case x of
    ('.':'/':_) -> tail x
    "." -> "/"
    _ -> x

directoryResponse :: GopherLogHandler -> FilePath -> IO GopherResponse
directoryResponse _ path =
  let makeItem :: Either a GopherFileType -> FilePath -> Either a GopherMenuItem
      makeItem t file = do
        fileType <- t
        pure $
          Item fileType (uEncode (takeFileName file)) file Nothing Nothing
   in do
     dir <- map (path </>) <$> getDirectoryContents path
     fileTypes <- mapM gopherFileType dir

     pure . MenuResponse . rights
       $ zipWith makeItem fileTypes (map makeAbsolute dir)

gophermapResponse :: GopherLogHandler -> FilePath -> IO GopherResponse
gophermapResponse logger path = do
  let gophermap = path </> ".gophermap"
  exists <- doesFileExist gophermap
  parsed <-
    if exists
      then parseOnly parseGophermap <$> B.readFile gophermap
      else pure $ Left "Gophermap file does not exist"
  case parsed of
    Left err -> do
      when exists . logger GopherLogLevelWarn
        $  "Could not parse gophermap at " <> toGopherLogStr gophermap
        <> ": " <> toGopherLogStr err
      directoryResponse logger path
    Right right -> pure
      $ gophermapToDirectoryResponse (makeAbsolute path) right

-- | True -> Just a
--   False -> Nothing
boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True  a = Just a
boolToMaybe False _ = Nothing
