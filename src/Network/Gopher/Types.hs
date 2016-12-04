module Network.Gopher.Types
  ( GopherFileType (..)
  , GopherResponse (..)
  , GopherMenuItem (..)
  , fileTypeToChar
  , charToFileType
  , isFile
  )
  where

import           Prelude               hiding (lookup)

import           Network.Gopher.Util

import           Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString as B
import           Data.Char             (ord, chr)
import           Data.Map              (Map (), fromList, lookup)
import           Data.Maybe            (fromJust, fromMaybe)
import           Data.Tuple            (swap)
import           Data.Word             (Word8 ())
import           Network.Socket        (PortNumber ())
import           System.FilePath       (splitPath, takeBaseName)

-- | a gopher menu item represented as type used in directory listings.
data GopherMenuItem = Item GopherFileType ByteString FilePath
  deriving (Show, Eq)

-- | type representation of a response of the gopher server
data GopherResponse = MenuResponse [GopherMenuItem]
  | FileResponse ByteString
  | ErrorResponse ByteString
  deriving (Show, Eq)

-- | converts the GopherResponse into a ByteString that can be sent to the
-- client.
-- | type representation of the defined gopher file types
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
  | InfoLine
  deriving (Show, Eq, Ord, Enum)

fileTypeToChar :: GopherFileType -> Word8
fileTypeToChar t = asciiOrd $
  case t of
    File -> '0'
    Directory -> '1'
    PhoneBookServer -> '2'
    Error -> '3'
    BinHexMacintoshFile -> '4'
    DOSArchive -> '5'
    UnixUuencodedFile -> '6'
    IndexSearchServer -> '7'
    TelnetSession -> '8'
    BinaryFile -> '9'
    RedundantServer -> '+'
    Tn3270Session -> 'T'
    GifFile -> 'g'
    ImageFile -> 'i'
    InfoLine -> 'I'

charToFileType :: Word8 -> GopherFileType
charToFileType c =
  case asciiChr c of
     '1' -> Directory
     '2' -> PhoneBookServer
     '3' -> Error
     '4' -> BinHexMacintoshFile
     '5' -> DOSArchive
     '6' -> UnixUuencodedFile
     '7' -> IndexSearchServer
     '8' -> TelnetSession
     '9' -> BinaryFile
     '+' -> RedundantServer
     'T' -> Tn3270Session
     'g' -> GifFile
     'i' -> ImageFile
     'I' -> InfoLine
     _   -> InfoLine -- default value

isFile :: GopherFileType -> Bool
isFile File = True
isFile BinHexMacintoshFile = True
isFile DOSArchive = True
isFile UnixUuencodedFile = True
isFile GifFile = True
isFile ImageFile = True
isFile _ = False
