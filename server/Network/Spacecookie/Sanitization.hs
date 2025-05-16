{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.Sanitization
  ( sanitizePath
  ) where

import System.FilePath.Posix.ByteString (RawFilePath, normalise, joinPath, splitPath, equalFilePath)

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
sanitizePath :: RawFilePath -> RawFilePath
sanitizePath =
  -- To retain prior behavior @"."@ after normalisation is mapped to @""@
  (\p -> if p == "." then "" else p)
  . joinPath
  . filter (\p -> not (equalFilePath p ".."))
  . splitPath . normalise
