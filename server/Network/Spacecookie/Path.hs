{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.Path
  ( sanitizePath
  , makeAbsolute
  ) where

import qualified Data.ByteString as B
import System.FilePath.Posix.ByteString (RawFilePath, normalise, joinPath, splitPath, equalFilePath, (</>))

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
sanitizePath :: RawFilePath -> RawFilePath
sanitizePath =
  -- To retain prior behavior @"."@ after normalisation is mapped to @""@
  (\p -> if p == "." then "" else p)
  . joinPath
  . filter (\p -> not (equalFilePath p ".."))
  . splitPath . normalise

-- | Convert a given path to an absolute path, treating it as if the current directory were the
--   root directory. The result is 'normalise'd. Absolute paths are not changed (except for the
--   normalisation).
makeAbsolute :: RawFilePath -> RawFilePath
makeAbsolute x = normalise $ "/" </> x
