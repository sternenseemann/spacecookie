module Network.Gopher.Types
  ( GopherFileType (..)
  , GopherResponse (..)
  , GopherMenuItem (..)
  , fileTypeToChar
  , charToFileType
  , isFile
  )
  where

import Prelude hiding (lookup)

import Data.ByteString (ByteString ())
import Data.Char (chr, ord)
import Data.Word (Word8 ())

-- | entry in a gopher menu
data GopherMenuItem
  = Item GopherFileType ByteString ByteString (Maybe ByteString) (Maybe Integer)
  -- ^ file type, menu text, selector, server name (optional), port (optional).
  --   None of the given 'ByteString's may contain tab characters.
  deriving (Show, Eq)

data GopherResponse
  = MenuResponse [GopherMenuItem] -- ^ gopher menu, wrapper around a list of 'GopherMenuItem's
  | FileResponse ByteString       -- ^ return the given 'ByteString' as a file
  | ErrorResponse ByteString      -- ^ gopher menu containing a single error with the given 'ByteString' as text
  deriving (Show, Eq)

-- | rfc-defined gopher file types plus info line and HTML
data GopherFileType
  = File                 -- ^ text file, default type
  | Directory            -- ^ a gopher menu
  | PhoneBookServer
  | Error                -- ^ error entry in menu
  | BinHexMacintoshFile
  | DOSArchive
  | UnixUuencodedFile
  | IndexSearchServer
  | TelnetSession
  | BinaryFile           -- ^ binary file
  | RedundantServer
  | Tn3270Session
  | GifFile              -- ^ gif
  | ImageFile            -- ^ image of any format
  | InfoLine             -- ^ menu entry without associated file
  | Html                 -- ^ Special type for HTML, most commonly used for <https://en.wikipedia.org/wiki/Gopher_%28protocol%29#URL_links links to other protocols>
  deriving (Show, Eq, Ord, Enum)

fileTypeToChar :: GopherFileType -> Word8
fileTypeToChar t = fromIntegral . ord $
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
    ImageFile -> 'I'
    InfoLine -> 'i'
    Html -> 'h'

charToFileType :: Word8 -> GopherFileType
charToFileType c =
  case chr (fromIntegral c) of
     '0' -> File
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
     'I' -> ImageFile
     'i' -> InfoLine
     'h' -> Html
     _   -> InfoLine -- default value

isFile :: GopherFileType -> Bool
isFile File = True
isFile BinHexMacintoshFile = True
isFile DOSArchive = True
isFile UnixUuencodedFile = True
isFile GifFile = True
isFile ImageFile = True
isFile _ = False
