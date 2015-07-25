module Spacecookie.Types
  ( GopherPath ()
  , GopherFileType (..)
  , GopherResponse (..)
  , GopherMenuItem (..)
  , fileTypeToChar
  , charToFileType
  , isFile
  , menuItem
  , combine
  , implode
  , response
  , constructGopherPath
  , destructGopherPath
  , canonicalizePath
  , gopherRequestToPath)
  where

import           Prelude               hiding (lookup)

import           Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import           Data.Map              (Map (), fromList, lookup)
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)
import           Network.Socket        (PortNumber ())
import           System.FilePath       (splitPath, takeBaseName)

-- | Internal representation of a gopher path
type GopherPath = [ByteString]

-- | concatenates two GopherPaths
combine :: GopherPath -> GopherPath -> GopherPath
combine = (++)

-- | Implode is intended for getting a path usable in the protocol from GopherPath
implode :: GopherPath -> FilePath
implode [] = "/"
implode path = unpack $ foldl (\acc p -> B.concat [acc, pack "/", p]) B.empty path

-- | destructGopherPath is intended to create a path usable for accessing the file
-- it makes use of the fact that wie chdir into our root.
destructGopherPath :: GopherPath -> FilePath
destructGopherPath = ("." ++) . implode

-- | builds a GopherPath from a regular FilePath
constructGopherPath :: FilePath -> GopherPath
constructGopherPath file = canonicalizePath $ map (dropSlash . pack) $ splitPath file
  where dropSlash x = if B.null x || B.last x /= '/'
                        then x
                        else B.init x

-- | builds a path to the requested file or directory
-- from a line sent by the gopher client.
gopherRequestToPath :: ByteString -> GopherPath
gopherRequestToPath line = constructGopherPath $ B.unpack line

-- | fixes some possible exploits by avoiding directory traversal.
canonicalizePath :: GopherPath -> GopherPath
canonicalizePath = filter (\part -> (not . B.null) part && part /= pack ".." && part /= pack ".")

-- | a gopher menu item represented as type used in directory listnings.
data GopherMenuItem = Item GopherFileType ByteString GopherPath ByteString PortNumber
  deriving (Show, Eq)

-- | convinience wrapper around the GopherMenuItem constructor
menuItem :: ByteString -> PortNumber -> GopherPath -> GopherFileType -> GopherMenuItem
menuItem server port fp ft = Item ft basename fp server port
  where basename = if null fp then pack "" else last fp

-- | type representation of a response of the gopher server
data GopherResponse = MenuResponse [GopherMenuItem]
  | FileResponse ByteString
  | ErrorResponse ByteString ByteString PortNumber
  deriving (Show, Eq)

-- | converts the GopherResponse into a ByteString that can be sent to the
-- client.
response :: GopherResponse -> ByteString
response (MenuResponse items) = foldl (\acc (Item fileType title path server port) -> B.append acc $
  fileTypeToChar fileType `B.cons` B.concat [title, pack "\t", pack $ implode path, pack "\t", server,
                                            pack "\t", pack $ show port, pack "\r\n"]) B.empty items
response (FileResponse str) = str
response (ErrorResponse reason server port) = fileTypeToChar Error `B.cons`
  B.concat [reason, pack "\tErr\t", server, pack "\t", pack $ show port, pack "\r\n"]

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

-- | holds the relation between the Type and the
-- characters used in the protocol
masterFileTypeCharRelation:: [(GopherFileType, Char)]
masterFileTypeCharRelation = [ (File, '0')
  , (Directory, '1')
  , (PhoneBookServer, '2')
  , (Error, '3')
  , (BinHexMacintoshFile, '4')
  , (DOSArchive, '5')
  , (UnixUuencodedFile, '6')
  , (IndexSearchServer, '7')
  , (TelnetSession, '8')
  , (BinaryFile, '9')
  , (RedundantServer, '+')
  , (Tn3270Session, 'T')
  , (GifFile, 'g')
  , (ImageFile, 'I')
  -- non-official filetypes
  , (InfoLine, 'i')
  ]

fileTypeToCharRelation :: Map GopherFileType Char
fileTypeToCharRelation = fromList masterFileTypeCharRelation

charToFileTypeRelation :: Map Char GopherFileType
charToFileTypeRelation = fromList $ map swap masterFileTypeCharRelation

-- | lookup function for fileTypeToCharRelation
fileTypeToChar :: GopherFileType -> Char
fileTypeToChar t = fromJust $ lookup t fileTypeToCharRelation

charToFileType :: Char -> GopherFileType
charToFileType c = fromJust $ lookup c charToFileTypeRelation

isFile :: GopherFileType -> Bool
isFile File = True
isFile BinHexMacintoshFile = True
isFile DOSArchive = True
isFile UnixUuencodedFile = True
isFile GifFile = True
isFile ImageFile = True
isFile _ = False
