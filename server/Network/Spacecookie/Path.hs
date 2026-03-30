{-# LANGUAGE QuasiQuotes #-}
module Network.Spacecookie.Path
  ( sanitizePath
  , makeAbsolute
  , containsDotFiles
  ) where

import System.OsPath.Posix (PosixPath, normalise, joinPath, splitPath, equalFilePath, (</>))
import System.OsString.Posix (isPrefixOf, pstr)

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
sanitizePath :: PosixPath -> PosixPath
sanitizePath =
  joinPath
  . filter (\p -> not (equalFilePath p [pstr|..|]))
  . splitPath . normalise

-- | Convert a given path to an absolute path, treating it as if the current directory were the
--   root directory. The result is 'normalise'd. Absolute paths are not changed (except for the
--   normalisation).
makeAbsolute :: PosixPath -> PosixPath
makeAbsolute x = normalise $ [pstr|/|] </> x

-- | Wether any components of the given path begin with a dot, although @.@ is
--   allowed.
containsDotFiles :: PosixPath -> Bool
containsDotFiles =
  any (\p -> [pstr|.|] `isPrefixOf` p && not (equalFilePath p [pstr|.|]))
  . splitPath
