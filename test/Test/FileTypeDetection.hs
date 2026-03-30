{-# LANGUAGE QuasiQuotes #-}
module Test.FileTypeDetection (fileTypeDetectionTests) where

import Network.Gopher (GopherFileType (..))
import Network.Spacecookie.FileType

import System.OsPath.Posix (takeExtension)
import System.OsString.Posix (pstr)
import Test.Tasty
import Test.Tasty.HUnit

fileTypeDetectionTests :: TestTree
fileTypeDetectionTests = testGroup "spacecookie server file type detection"
  [ ioTests
  , suffixTests
  ]

ioTests :: TestTree
ioTests = testCase "gopherFileType tests" $ do
  assertEqual "Fallback to File without extension" (Right File)
    =<< gopherFileType [pstr|LICENSE|]

  assertEqual "non-existent dot files are forbidden" (Left PathIsNotAllowed)
    =<< gopherFileType [pstr|.dot-file-missing|]

  assertEqual "dot file along the path" (Left PathIsNotAllowed)
    =<< gopherFileType [pstr|.git/HEAD|]

  assertEqual "dot file along the path" (Left PathIsNotAllowed)
    =<< gopherFileType [pstr|./foo/.git/HEAD|]

  assertEqual ".. is disallowed" (Left PathIsNotAllowed)
    =<< gopherFileType [pstr|/lol/../../doing/directory/traversal.txt|]

  assertEqual "\".\" is allowed" (Right Directory)
    =<< gopherFileType [pstr|.|]

  assertEqual "txt files" (Right File)
    =<< gopherFileType [pstr|./docs/rfc1436.txt|]

  assertEqual "missing file" (Left PathDoesNotExist)
    =<< gopherFileType [pstr|missing/this.txt|]

suffixTests :: TestTree
suffixTests = testCase "correct mapping of suffixes" $ do
  assertEqual "BinHexMacintoshFile" BinHexMacintoshFile $
    lookupSuffix $ takeExtension [pstr|test.hqx|]

  assertEqual "tar.gz is BinaryFile" BinaryFile $
    lookupSuffix $ takeExtension [pstr|/releases/spacecookie-0.3.0.0.tar.gz|]

  assertEqual "gif file" GifFile $
    lookupSuffix $ takeExtension [pstr|funny.gif|]

  mapM_ (assertEqual "image file" ImageFile . lookupSuffix . takeExtension)
    [ [pstr|hello.png|], [pstr|/my/beautiful.jpg|], [pstr|./../lol.jpeg|]
    , [pstr|../bar.tif|], [pstr|my.tiff|], [pstr|.hidden.svg|], [pstr|my.bmp|] ]

  assertEqual "fallback to File" File $
    lookupSuffix $ takeExtension [pstr|my/unknown.strange-extension|]
