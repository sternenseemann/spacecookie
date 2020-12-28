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

-- | Convert a gophermap to a gopher menu response.
gophermapToDirectoryResponse :: Gophermap -> GopherResponse
gophermapToDirectoryResponse entries =
  MenuResponse (map gophermapEntryToMenuItem entries)

gophermapEntryToMenuItem :: GophermapEntry -> GopherMenuItem
gophermapEntryToMenuItem (GophermapEntry ft desc path host port) =
  Item ft desc (fromMaybe (uDecode desc) path) host port

fileTypeChars :: [Char]
fileTypeChars = "0123456789+TgIih"

-- | A gophermap entry makes all values of a gopher menu item optional except for file type and description. When converting to a 'GopherMenuItem', appropriate default values are used.
data GophermapEntry = GophermapEntry
  GopherFileType ByteString
  (Maybe FilePath) (Maybe ByteString) (Maybe Integer) -- ^ file type, description, path, server name, port number
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
    (santinizeIfNotUrl . fst . U.decode . unpack <$> pathString)
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
    (santinizeIfNotUrl . fst . U.decode . unpack <$> pathString)
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
