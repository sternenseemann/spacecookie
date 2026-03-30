{-# LANGUAGE QuasiQuotes #-}
module Network.Spacecookie.FileType
  ( PathError (..)
  , gopherFileType
  -- exposed for tests
  , lookupSuffix
  , checkNoDotFiles
  ) where

import Network.Spacecookie.Path (containsDotFiles)

import Data.Char (ord, toLower)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Network.Gopher (GopherFileType (..))
import System.Directory.OsPath (doesDirectoryExist, doesFileExist)
import System.OsPath.Posix (PosixPath, takeExtension)
import System.OsString.Posix (PosixString, pstr)
import qualified System.OsString.Posix as Posix
import System.OsString.Internal.Types (OsString (..), PosixChar (..))

fileTypeMap :: M.Map PosixString GopherFileType
fileTypeMap = M.fromList
  [ ([pstr|.gif|], GifFile)
  , ([pstr|.png|], ImageFile)
  , ([pstr|.jpg|], ImageFile)
  , ([pstr|.jpeg|], ImageFile)
  , ([pstr|.tiff|], ImageFile)
  , ([pstr|.tif|], ImageFile)
  , ([pstr|.bmp|], ImageFile)
  , ([pstr|.webp|], ImageFile)
  , ([pstr|.apng|], ImageFile)
  , ([pstr|.mng|], ImageFile)
  , ([pstr|.heif|], ImageFile)
  , ([pstr|.heifs|], ImageFile)
  , ([pstr|.heic|], ImageFile)
  , ([pstr|.heics|], ImageFile)
  , ([pstr|.avci|], ImageFile)
  , ([pstr|.avcs|], ImageFile)
  , ([pstr|.avif|], ImageFile)
  , ([pstr|.avifs|], ImageFile)
  , ([pstr|.ico|], ImageFile)
  , ([pstr|.svg|], ImageFile)
  , ([pstr|.raw|], ImageFile) -- TODO: RAW files should maybe be binary files?
  , ([pstr|.cr2|], ImageFile)
  , ([pstr|.nef|], ImageFile)
  , ([pstr|.json|], File)
  , ([pstr|.txt|], File)
  , ([pstr|.text|], File)
  , ([pstr|.md|], File)
  , ([pstr|.mdown|], File)
  , ([pstr|.mkdn|], File)
  , ([pstr|.mkd|], File)
  , ([pstr|.markdown|], File)
  , ([pstr|.adoc|], File)
  , ([pstr|.rst|], File)
  , ([pstr|.zip|], BinaryFile)
  , ([pstr|.tar|], BinaryFile)
  , ([pstr|.gz|], BinaryFile)
  , ([pstr|.bzip2|], BinaryFile)
  , ([pstr|.xz|], BinaryFile)
  , ([pstr|.tgz|], BinaryFile)
  , ([pstr|.doc|], BinaryFile)
  , ([pstr|.hqx|], BinHexMacintoshFile)
  ]

-- | Transform a 'Word8' to lowercase if the solution is in bounds.
--
--   >>> asciiToLower 65
--   97
--   >>> asciiToLower 97
--   97
--   >>> asciiToLower 220
--   220
--   >>> asciiToLower 252
--   252
asciiToLower :: PosixChar -> PosixChar
asciiToLower orig
  | getPosixChar orig > 127 || ord lower > 127 = orig
  | otherwise = Posix.unsafeFromChar lower
  where lower :: Char
        lower = toLower $ Posix.toChar orig

lookupSuffix :: PosixPath -> GopherFileType
lookupSuffix = fromMaybe File
  . (flip M.lookup) fileTypeMap
  . Posix.map asciiToLower

data PathError
  = PathDoesNotExist
  | PathIsNotAllowed
  deriving (Show, Eq, Ord, Enum)

-- | Action in the 'Either' monad which causes a
--   failure if there's any dot files or directory
--   in the given path
checkNoDotFiles :: PosixPath -> Either PathError ()
checkNoDotFiles path
  | containsDotFiles path = Left  PathIsNotAllowed
  | otherwise = Right ()

-- | calculates the file type identifier used in the Gopher
--   protocol for a given file and returns a descriptive error
--   if the file is not accessible or a dot file (and thus not
--   allowed to access)
gopherFileType :: PosixPath -> IO (Either PathError GopherFileType)
gopherFileType path = (checkNoDotFiles path >>) <$> do
  -- we only support Posix for now, so failing to compile on Windows is not an issue
  let os = OsString path
  isDir <- doesDirectoryExist os
  if isDir
    then pure $ Right Directory
    else do
      fileExists <- doesFileExist os
      pure $
        if fileExists
          then Right $ lookupSuffix $ takeExtension path
          else Left  $ PathDoesNotExist
