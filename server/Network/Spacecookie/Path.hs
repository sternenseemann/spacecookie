{-# LANGUAGE OverloadedStrings #-}
module Network.Spacecookie.Path
  ( sanitizePath
  , makeAbsolute
  ) where

import Network.Gopher.Util (boolToMaybe)

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import System.FilePath.Posix.ByteString (RawFilePath, normalise, joinPath, splitPath, equalFilePath)

-- | Normalise a path and prevent <https://en.wikipedia.org/wiki/Directory_traversal_attack directory traversal attacks>.
sanitizePath :: RawFilePath -> RawFilePath
sanitizePath =
  -- To retain prior behavior @"."@ after normalisation is mapped to @""@
  (\p -> if p == "." then "" else p)
  . joinPath
  . filter (\p -> not (equalFilePath p ".."))
  . splitPath . normalise

makeAbsolute :: RawFilePath -> RawFilePath
makeAbsolute x = fromMaybe x
  $   boolToMaybe ("./" `B.isPrefixOf` x) (B.tail x)
  <|> boolToMaybe ("." == x) "/"