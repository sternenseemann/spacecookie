{-|
Module      : Network.Gopher.Util.Gophermap
Stability   : experimental
Portability : POSIX

This module implements a parser for <https://raw.githubusercontent.com/sternenseemann/spacecookie/master/docs/gophermap-pygopherd.txt gophermap files>.

Example usage:

@
import Network.Gopher.Util.Gophermap
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

main = do
  file <- B.readFile "gophermap"
  print $ parseOnly parseGophermap file
@


-}

{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util.Gophermap (
    parseGophermap
  , GophermapEntry (..)
  , Gophermap
  , gophermapToDirectoryResponse
  ) where

import Prelude hiding (take, takeWhile)

import Network.Gopher.Types
import Network.Gopher.Util

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString (), pack, unpack)
import Data.Maybe (fromMaybe)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())
import System.FilePath.Posix ((</>))

-- | Given a directory and a Gophermap contained within it,
--   return the corresponding gopher menu response.
gophermapToDirectoryResponse :: FilePath -> Gophermap -> GopherResponse
gophermapToDirectoryResponse dir entries =
  MenuResponse (map (gophermapEntryToMenuItem dir) entries)

gophermapEntryToMenuItem :: FilePath -> GophermapEntry -> GopherMenuItem
gophermapEntryToMenuItem dir (GophermapEntry ft desc path host port) =
  Item ft desc (fromMaybe (uDecode desc) (realPath <$> path)) host port
  where realPath p =
          case p of
            GophermapAbsolute p' -> p'
            GophermapRelative p' -> dir </> p'
            GophermapUrl u       -> u

fileTypeChars :: [Char]
fileTypeChars = "0123456789+TgIih"

-- | Wrapper around 'FilePath' to indicate whether it is
--   relative or absolute.
data GophermapFilePath
  = GophermapAbsolute FilePath
  | GophermapRelative FilePath
  | GophermapUrl String
  deriving (Show, Eq)

-- | Take 'ByteString' from gophermap, decode it,
--   sanitize and determine path type.
--
--   * Gophermap paths that start with a slash are
--     considered to be absolute.
--   * Gophermap paths that start with "URL:" are
--     considered as an external URL and left as-is.
--   * everything else is considered a relative path
makeGophermapFilePath :: ByteString -> GophermapFilePath
makeGophermapFilePath b =
  case bytes of
    -- starts with "URL:"
    (85:82:76:58:_) -> GophermapUrl . fst $ U.decode bytes
    -- starts with '/'
    (47:_) -> GophermapAbsolute $ processPath bytes
    _      -> GophermapRelative $ processPath bytes
  where processPath = sanitizePath . fst . U.decode
        bytes = unpack b

-- | A gophermap entry makes all values of a gopher menu item optional except for file type and description. When converting to a 'GopherMenuItem', appropriate default values are used.
data GophermapEntry = GophermapEntry
  GopherFileType ByteString
  (Maybe GophermapFilePath) (Maybe ByteString) (Maybe Integer) -- ^ file type, description, path, server name, port number
  deriving (Show, Eq)

type Gophermap = [GophermapEntry]

-- | Attoparsec 'Parser' for the <https://raw.githubusercontent.com/sternenseemann/spacecookie/master/docs/gophermap-pygopherd.txt gophermap file format>
parseGophermap :: Parser Gophermap
parseGophermap = many1 parseGophermapLine <* endOfInput

gopherFileTypeChar :: Parser Word8
gopherFileTypeChar = satisfy (inClass fileTypeChars)

parseGophermapLine :: Parser GophermapEntry
parseGophermapLine = emptyGophermapline <|>
                     regularGophermapline <|>
                     infoGophermapline <|>
                     gophermaplineWithoutFileTypeChar

infoGophermapline :: Parser GophermapEntry
infoGophermapline = do
  text <- takeWhile1 (notInClass "\t\r\n")
  endOfLineOrInput
  return $ GophermapEntry InfoLine
    text
    Nothing
    Nothing
    Nothing

regularGophermapline :: Parser GophermapEntry
regularGophermapline = do
  fileTypeChar <- gopherFileTypeChar
  text <- itemValue
  pathString <- optionalValue
  host <- optionalValue
  portString <- optionalValue
  endOfLineOrInput
  return $ GophermapEntry (charToFileType fileTypeChar)
    text
    (makeGophermapFilePath <$> pathString)
    host
    (byteStringToPort <$> portString)

emptyGophermapline :: Parser GophermapEntry
emptyGophermapline = do
  endOfLine'
  return emptyInfoLine
    where emptyInfoLine = GophermapEntry InfoLine (pack []) Nothing Nothing Nothing

gophermaplineWithoutFileTypeChar :: Parser GophermapEntry
gophermaplineWithoutFileTypeChar = do
  text <- itemValue
  pathString <- optionalValue
  host <- optionalValue
  portString <- optionalValue
  endOfLineOrInput
  return $ GophermapEntry InfoLine
    text
    (makeGophermapFilePath <$> pathString)
    host
    (byteStringToPort <$> portString)

byteStringToPort :: ByteString -> Integer
byteStringToPort s = read . fst . U.decode . unpack $ s

optionalValue :: Parser (Maybe ByteString)
optionalValue = option Nothing $ do
  _ <- satisfy (inClass "\t")
  Just <$> itemValue

itemValue :: Parser ByteString
itemValue = takeWhile1 (notInClass "\t\r\n")

endOfLine' :: Parser ()
endOfLine' = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

endOfLineOrInput :: Parser ()
endOfLineOrInput = endOfInput <|> endOfLine'
