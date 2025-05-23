{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.FileType
  ( PathError (..)
  , gopherFileType
  -- exposed for tests
  , lookupSuffix
  , checkNoDotFiles
  ) where

import Network.Spacecookie.Path (containsDotFiles)

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Network.Gopher (GopherFileType (..))
import Network.Gopher.Util (asciiToLower)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath.Posix.ByteString ( RawFilePath, takeExtension
                                        , decodeFilePath)

fileTypeMap :: M.Map RawFilePath GopherFileType
fileTypeMap = M.fromList
  [ (".gif", GifFile)
  , (".png", ImageFile)
  , (".jpg", ImageFile)
  , (".jpeg", ImageFile)
  , (".tiff", ImageFile)
  , (".tif", ImageFile)
  , (".bmp", ImageFile)
  , (".webp", ImageFile)
  , (".apng", ImageFile)
  , (".mng", ImageFile)
  , (".heif", ImageFile)
  , (".heifs", ImageFile)
  , (".heic", ImageFile)
  , (".heics", ImageFile)
  , (".avci", ImageFile)
  , (".avcs", ImageFile)
  , (".avif", ImageFile)
  , (".avifs", ImageFile)
  , (".ico", ImageFile)
  , (".svg", ImageFile)
  , (".raw", ImageFile) -- TODO: RAW files should maybe be binary files?
  , (".cr2", ImageFile)
  , (".nef", ImageFile)
  , (".json", File)
  , (".txt", File)
  , (".text", File)
  , (".md", File)
  , (".mdown", File)
  , (".mkdn", File)
  , (".mkd", File)
  , (".markdown", File)
  , (".adoc", File)
  , (".rst", File)
  , (".zip", BinaryFile)
  , (".tar", BinaryFile)
  , (".gz", BinaryFile)
  , (".bzip2", BinaryFile)
  , (".xz", BinaryFile)
  , (".tgz", BinaryFile)
  , (".doc", BinaryFile)
  , (".hqx", BinHexMacintoshFile)
  ]

lookupSuffix :: RawFilePath -> GopherFileType
lookupSuffix = fromMaybe File
  . (flip M.lookup) fileTypeMap
  . B.map asciiToLower

data PathError
  = PathDoesNotExist
  | PathIsNotAllowed
  deriving (Show, Eq, Ord, Enum)

-- | Action in the 'Either' monad which causes a
--   failure if there's any dot files or directory
--   in the given path
checkNoDotFiles :: RawFilePath -> Either PathError ()
checkNoDotFiles path
  | containsDotFiles path = Left  PathIsNotAllowed
  | otherwise = Right ()

-- | calculates the file type identifier used in the Gopher
--   protocol for a given file and returns a descriptive error
--   if the file is not accessible or a dot file (and thus not
--   allowed to access)
gopherFileType :: RawFilePath -> IO (Either PathError GopherFileType)
gopherFileType path = (checkNoDotFiles path >>) <$> do
  let pathWide = decodeFilePath path
  isDir <- doesDirectoryExist pathWide
  if isDir
    then pure $ Right Directory
    else do
      fileExists <- doesFileExist pathWide
      pure $
        if fileExists
          then Right $ lookupSuffix $ takeExtension path
          else Left  $ PathDoesNotExist
