{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher.Util.Gophermap
  ( gophermapToDirectoryResponse
  , parseGophermap
  , Gophermap (..))
    where

import Prelude hiding (take, takeWhile)

import Network.Gopher.Types
import Network.Gopher.Util

import Control.Applicative (many, (<$>), (<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString (), append, empty,
                        pack, singleton, unpack)
import qualified Data.String.UTF8 as U
import Data.Word (Word8 ())
import Network.Socket (PortNumber ())

-- | gophermapToDirectoryResponse adds the missing values of the
-- parsedGophermap so that it can used to construct a response for
-- the client
gophermapToDirectoryResponse :: Gophermap -> GopherResponse
gophermapToDirectoryResponse entries =
  MenuResponse (map gophermapEntryToMenuItem entries)

-- | converts a GophermapEntry to a GopherMenuItem and adds the missing
-- information
gophermapEntryToMenuItem :: GophermapEntry -> GopherMenuItem
gophermapEntryToMenuItem (GophermapEntry ft desc path host port) =
  Item ft desc (replaceIfNothing path (uDecode desc))
  where replaceIfNothing Nothing  r = r
        replaceIfNothing (Just x) _ = x

fileTypeChars :: [Char]
fileTypeChars = "123456789+TgIi"

data GophermapEntry = GophermapEntry
  GopherFileType ByteString
  (Maybe FilePath) (Maybe ByteString) (Maybe PortNumber)
  deriving (Show, Eq)

type Gophermap = [GophermapEntry]

parseGophermap :: Parser Gophermap
parseGophermap = many parseGophermapLine

-- | inline if version
if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

gopherFileTypeChar :: Parser Word8
gopherFileTypeChar = satisfy (inClass fileTypeChars)

parseGophermapLine :: Parser GophermapEntry
parseGophermapLine = emptyGophermapline <|>
                     infoGophermapline <|>
                     regularGophermapline <|>
                     gophermaplineWithoutFileTypeChar

infoGophermapline :: Parser GophermapEntry
infoGophermapline = do
  text <- takeWhile (notInClass "\t\r\n")
  endOfLine'
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
    endOfLine'
    return $ GophermapEntry (charToFileType fileTypeChar)
      text
      (santinizePath . fst . U.decode . unpack <$> pathString)
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
  endOfLine'
  return $ GophermapEntry InfoLine
    text
    (santinizePath . fst . U.decode . unpack <$> pathString)
    host
    (byteStringToPort <$> portString)

byteStringToPort :: ByteString -> PortNumber
byteStringToPort s = fromIntegral . read . fst . U.decode . unpack $ s

optionalValue :: Parser (Maybe ByteString)
optionalValue = option Nothing $ do
  satisfy (inClass "\t")
  Just <$> itemValue

itemValue :: Parser ByteString
itemValue = takeTill (inClass "\t\r\n")

endOfLine' :: Parser ()
endOfLine' = (word8 10 >> return ()) <|> (string "\r\n" >> return ())
