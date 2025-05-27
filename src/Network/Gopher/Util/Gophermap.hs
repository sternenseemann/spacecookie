{-|
Module      : Network.Gopher.Util.Gophermap
Stability   : experimental
Portability : POSIX

This module implements a parser for gophermap files.

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
  , GophermapFilePath (..)
  , Gophermap
  , gophermapToDirectoryResponse
  ) where

import Prelude hiding (take, takeWhile)

import Network.Gopher.Types

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isDigit_w8)
import Data.ByteString (ByteString (), pack, unpack, isPrefixOf)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Word (Word8 ())
import System.FilePath.Posix.ByteString (RawFilePath, (</>), isAbsolute, normalise)
import Text.Read (readEither)

-- | Given a directory and a Gophermap contained within it,
--   return the corresponding gopher menu response.
gophermapToDirectoryResponse :: RawFilePath -> Gophermap -> GopherResponse
gophermapToDirectoryResponse dir entries =
  MenuResponse (map (gophermapEntryToMenuItem dir) entries)

gophermapEntryToMenuItem :: RawFilePath -> GophermapEntry -> GopherMenuItem
gophermapEntryToMenuItem dir (GophermapEntry ft desc path host port) =
  Item ft desc (fromMaybe desc (realPath <$> path)) host port
  where realPath p =
          case p of
            GophermapAbsolute p' -> p'
            -- TODO: `..` should be resolved textually for linking to the
            -- parent directory (if possible)
            GophermapRelative p' -> dir </> p'
            GophermapUrl u       -> u

fileTypeChars :: [Char]
fileTypeChars = "0123456789+TgIih"

-- | Wrapper around 'RawFilePath' to indicate whether it is
--   relative or absolute.
data GophermapFilePath
  = GophermapAbsolute RawFilePath -- ^ Absolute path starting with @/@
  | GophermapRelative RawFilePath -- ^ Relative path
  | GophermapUrl RawFilePath      -- ^ URL to another protocol starting with @URL:@
  deriving (Show, Eq)

-- | Take selector 'ByteString' from gophermap and
--   determine its 'GophermapFilePath' type.
--   Relative and absolute paths are 'normalised',
--   URLs passed on as is.
--
--   * Selectors that start with @"URL:"@ are considered
--     an external URL and left as-is.
--   * Absolute paths are identified by 'isAbsolute'.
--   * Everything else is considered a relative path.
--
--   Paths are 'normalise'-d, but not subject to any other
--   processing.
makeGophermapFilePath :: ByteString -> GophermapFilePath
makeGophermapFilePath b
  | "URL:" `isPrefixOf` b = GophermapUrl b
  | isAbsolute b = GophermapAbsolute normalisedPath
  | otherwise = GophermapRelative normalisedPath
  where
    normalisedPath = normalise b

-- | A gophermap entry makes all values of a gopher menu item optional except for file type and description. When converting to a 'GopherMenuItem', appropriate default values are used.
data GophermapEntry = GophermapEntry
  GopherFileType ByteString
  (Maybe GophermapFilePath) (Maybe ByteString) (Maybe Integer) -- ^ file type, description, path, server name, port number
  deriving (Show, Eq)

type Gophermap = [GophermapEntry]

-- | Attoparsec 'Parser' for the gophermap file format
parseGophermap :: Parser Gophermap
parseGophermap = many1 parseGophermapLine <* endOfInput

gopherFileTypeChar :: Parser Word8
gopherFileTypeChar = satisfy (inClass fileTypeChars)

parseGophermapLine :: Parser GophermapEntry
parseGophermapLine = emptyGophermapline
  <|> regularGophermapline
  <|> infoGophermapline

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
  _ <- satisfy (inClass "\t")
  pathString <- option Nothing $ Just <$> itemValue
  host <- optionalValue
  port <- optional portValue
  endOfLineOrInput
  return $ GophermapEntry (charToFileType fileTypeChar)
    text
    (makeGophermapFilePath <$> pathString)
    host
    port

emptyGophermapline :: Parser GophermapEntry
emptyGophermapline = do
  endOfLine'
  return emptyInfoLine
    where emptyInfoLine = GophermapEntry InfoLine (pack []) Nothing Nothing Nothing

portValue :: Parser Integer
portValue = do
  digits <- takeWhile1 isDigit_w8
  -- we know digits is just ASCII characters ([0-9])
  case readEither (map (chr . fromIntegral) (unpack digits)) of
    Left e -> fail e
    Right p -> pure p

optionalValue :: Parser (Maybe ByteString)
optionalValue = optional itemValue

optional :: Parser a -> Parser (Maybe a)
optional parser = option Nothing $ do
  _ <- satisfy (inClass "\t")
  Just <$> parser

itemValue :: Parser ByteString
itemValue = takeWhile1 (notInClass "\t\r\n")

endOfLine' :: Parser ()
endOfLine' = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

endOfLineOrInput :: Parser ()
endOfLineOrInput = endOfInput <|> endOfLine'
