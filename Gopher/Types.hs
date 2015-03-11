module Gopher.Types
  ( GopherPath ()
  , GopherFileType (..)
  , GopherResponse (..)
  , fileTypeToChar
  , combine
  , implode
  , response
  , constructGopherPath
  , destructGopherPath
  , canonicalizePath
  , gopherRequestToPath)
  where

import           Prelude                   hiding (FilePath (), lookup)

import           Data.ByteString.Char8     (ByteString, pack, unpack)
import qualified Data.ByteString.Char8     as B
import           Data.Map                  (Map (), fromList, lookup)
import           Data.Maybe                (fromJust)
import           Filesystem.Path.CurrentOS (FilePath ())
import qualified Filesystem.Path.CurrentOS as F
import           Network.Socket            (PortNumber ())

-- GopherPath
type GopherPath = [ByteString]

combine :: GopherPath -> GopherPath -> GopherPath
combine = (++)

implode :: GopherPath -> FilePath
implode [] = F.decodeString "/"
implode path = F.decode $ foldl (\acc p -> B.concat [acc, pack "/", p]) B.empty path

destructGopherPath :: FilePath -> GopherPath -> FilePath
destructGopherPath root path = root `F.append` implode path

constructGopherPath :: FilePath -> FilePath -> GopherPath
constructGopherPath _ file = canonicalizePath $ map (dropSlash . F.encode) $ F.splitDirectories file
  where dropSlash x = if B.null x || B.last x /= '/'
                        then x
                        else B.init x

gopherRequestToPath :: ByteString -> GopherPath
gopherRequestToPath line = constructGopherPath F.empty $ F.decode line

-- drops all '..' and '.' because we don't allow them
canonicalizePath :: GopherPath -> GopherPath
canonicalizePath = filter (not . B.all (== '.'))

-- GopherMenuItem
data GopherMenuItem = Item GopherFileType ByteString GopherPath ByteString PortNumber
  deriving (Show, Eq)

-- GopherResponse
data GopherResponse = MenuResponse [GopherMenuItem]
  | FileResponse ByteString
  | ErrorResponse ByteString ByteString PortNumber
  deriving (Show, Eq)

response :: GopherResponse -> ByteString
response (MenuResponse items) = foldl (\acc (Item fileType title path server port) -> B.append acc $
  fileTypeToChar fileType `B.cons` B.concat [title, F.encode $ implode path, server,
                                            pack $ show port, pack "\r\n"]) B.empty items
response (FileResponse str) = str
response (ErrorResponse reason server port) = fileTypeToChar Error `B.cons`
  B.concat [reason, pack "\tErr\t", server, pack $ show port, pack "\r\n"]

-- GopherFileType
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
  deriving (Show, Eq, Ord, Enum)

fileTypeToCharRelation :: Map GopherFileType Char
fileTypeToCharRelation = fromList [ (File, '0')
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
  , (ImageFile, 'I') ]

fileTypeToChar :: GopherFileType -> Char
fileTypeToChar t = fromJust $ lookup t fileTypeToCharRelation
