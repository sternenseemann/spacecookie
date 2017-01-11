{-# LANGUAGE OverloadedStrings #-}
import Config
import Network.Gopher
import Network.Gopher.Util (santinizePath, uEncode)
import Network.Gopher.Util.Gophermap
import Data.ByteString (ByteString ())
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (unless, filterM, sequence, join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment
import System.FilePath.Posix (takeFileName, takeExtension, (</>), dropDrive, splitDirectories)
import System.Posix.Directory (changeWorkingDirectory)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ configFile ] -> do
      doesFileExist configFile >>= (flip unless) (error "could not open config file")
      config' <- decode <$> BL.readFile configFile
      case config' of
        Just config -> do
          changeWorkingDirectory (rootDirectory config)
          runGopher (GopherConfig (serverName config) (serverPort config) ((Just (runUserName config)))) spacecookie
        Nothing -> error "failed to parse config"
    _ -> error "config file must be given"

spacecookie :: String -> IO GopherResponse
spacecookie path' = do
  let path = "." </> dropDrive (santinizePath path')
  fileType <- gopherFileType path
  pathType <- pathType path

  if not (isListable pathType path')
    then pure . ErrorResponse $ "Accessing '" ++ path' ++ "' is not allowed."
    else case fileType of
           Error -> pure $
             if "URL:" `isPrefixOf` path'
               then ErrorResponse $ "spacecookie does not support proxying HTTP, try using a gopher client that supports the h-type. If you tried to request a file called '" ++ path' ++ "', it does not exist."
               else ErrorResponse $ "The requested file '" ++ path' ++ "' does not exist or is not available."
           -- always use gophermapResponse which falls back
           -- to directoryResponse if there is no gophermap file
           Directory -> gophermapResponse path
           _ -> fileResponse path

fileResponse :: FilePath -> IO GopherResponse
fileResponse path = FileResponse <$> B.readFile path

makeAbsolute :: FilePath -> FilePath
makeAbsolute x = if "./" `isPrefixOf` x
                   then tail x
                   else x

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
    Right right -> pure $ gophermapToDirectoryResponse right

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
