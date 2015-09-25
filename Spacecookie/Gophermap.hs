{-# LANGUAGE OverloadedStrings #-}
module Spacecookie.Gophermap where

import           Prelude                          hiding (take, takeWhile)

import           Spacecookie.ConfigParsing
import           Spacecookie.Monad
import           Spacecookie.Types

import           Control.Applicative              (many, (<$>), (<|>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (ask)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            (ByteString (), append, empty,
                                                   pack, singleton, unpack)
import           Data.Maybe                       (Maybe (..))
import           Data.Word                        (Word8 ())
import           Network.Socket                   (PortNumber ())

data GophermapEntry = GophermapEntry GopherFileType ByteString (Maybe GopherPath) (Maybe ByteString) (Maybe PortNumber)
  deriving (Show, Eq)

type Gophermap = [GophermapEntry]

parseGophermap :: Parser Gophermap
parseGophermap = many parseGophermapLine

-- | inline if version
if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

gopherFileTypeChar :: Parser Char
gopherFileTypeChar = satisfy (inClass fileTypeChars)

parseGophermapLine :: Parser GophermapEntry
parseGophermapLine = emptyGophermapline <|> regularGophermapline <|> gophermaplineWithoutFileTypeChar

regularGophermapline :: Parser GophermapEntry
regularGophermapline = do
    fileTypeChar <- gopherFileTypeChar
    text <- itemValue
    pathString <- optionalValue
    host <- optionalValue
    portString <- optionalValue
    endOfLine
    return $ GophermapEntry (charToFileType fileTypeChar)
      text
      (gopherRequestToPath <$> pathString)
      host
      (byteStringToPort <$> portString)

emptyGophermapline :: Parser GophermapEntry
emptyGophermapline = do
  endOfLine
  return emptyInfoLine
    where emptyInfoLine = GophermapEntry InfoLine (pack "") Nothing Nothing Nothing

gophermaplineWithoutFileTypeChar :: Parser GophermapEntry
gophermaplineWithoutFileTypeChar = do
  text <- itemValue
  pathString <- optionalValue
  host <- optionalValue
  portString <- optionalValue
  endOfLine
  return $ GophermapEntry InfoLine
    text
    (gopherRequestToPath <$> pathString)
    host
    (byteStringToPort <$> portString)

byteStringToPort :: ByteString -> PortNumber
byteStringToPort s = fromIntegral $ read $ unpack s

optionalValue :: Parser (Maybe ByteString)
optionalValue = option Nothing $ do
  "\t"
  Just <$> itemValue

itemValue :: Parser ByteString
itemValue = takeTill (inClass "\t\r\n")
