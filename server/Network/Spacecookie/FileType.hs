module Network.Spacecookie.FileType
  ( PathError (..)
  , gopherFileType
  ) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Network.Gopher (GopherFileType (..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath.Posix (takeExtension, splitDirectories)

fileTypeMap :: M.Map FilePath GopherFileType
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

lookupSuffix :: FilePath -> GopherFileType
lookupSuffix = fromMaybe File
  . (flip M.lookup) fileTypeMap
  . map toLower

data PathError
  = PathDoesNotExist
  | PathIsNotAllowed
  deriving (Show, Eq, Ord, Enum)

-- | Action in the 'Either' monad which causes a
--   failure if there's any dot files or directory
--   in the given path
checkNoDotFiles :: FilePath -> Either PathError ()
checkNoDotFiles path = do
  let segments = splitDirectories $
        case path of
          -- this prevents relative directories
          -- from being forbidden while singular
          -- '.' in the path somewhere get
          -- flagged and "." stays allowed.
          ('.':'/':_) -> tail path
          "."         -> ""
          _           -> path

  if any ((== ".") . take 1) segments
    then Left  PathIsNotAllowed
    else Right ()

-- | calculates the file type identifier used in the Gopher
--   protocol for a given file and returns a descriptive error
--   if the file is not accessible or a dot file (and thus not
--   allowed to access)
gopherFileType :: FilePath -> IO (Either PathError GopherFileType)
gopherFileType path = (checkNoDotFiles path >>) <$> do
  isDir <- doesDirectoryExist path
  if isDir
    then pure $ Right Directory
    else do
      fileExists <- doesFileExist path
      pure $
        if fileExists
          then Right $ lookupSuffix $ takeExtension path
          else Left  $ PathDoesNotExist
