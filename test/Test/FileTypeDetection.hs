module Test.FileTypeDetection (fileTypeDetectionTests) where

import Network.Gopher (GopherFileType (..))
import Network.Spacecookie.FileType

import System.FilePath.Posix (takeExtension)
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
    =<< gopherFileType "LICENSE"

  assertEqual "non-existent dot files are forbidden" (Left PathIsNotAllowed)
    =<< gopherFileType ".dot-file-missing"

  assertEqual "dot file along the path" (Left PathIsNotAllowed)
    =<< gopherFileType ".git/HEAD"

  assertEqual "dot file along the path" (Left PathIsNotAllowed)
    =<< gopherFileType "./foo/.git/HEAD"

  assertEqual ".. is disallowed" (Left PathIsNotAllowed)
    =<< gopherFileType "/lol/../../doing/directory/traversal.txt"

  assertEqual "\".\" is allowed" (Right Directory)
    =<< gopherFileType "."

  assertEqual "txt files" (Right File)
    =<< gopherFileType "./docs/rfc1436.txt"

  assertEqual "missing file" (Left PathDoesNotExist)
    =<< gopherFileType "missing/this.txt"

suffixTests :: TestTree
suffixTests = testCase "correct mapping of suffixes" $ do
  assertEqual "BinHexMacintoshFile" BinHexMacintoshFile $
    lookupSuffix $ takeExtension "test.hqx"

  assertEqual "tar.gz is BinaryFile" BinaryFile $
    lookupSuffix $ takeExtension "/releases/spacecookie-0.3.0.0.tar.gz"

  assertEqual "gif file" GifFile $
    lookupSuffix $ takeExtension "funny.gif"

  mapM_ (assertEqual "image file" ImageFile . lookupSuffix . takeExtension)
    [ "hello.png", "/my/beautiful.jpg", "./../lol.jpeg"
    , "../bar.tif", "my.tiff", ".hidden.svg", "my.bmp" ]

  assertEqual "fallback to File" File $
    lookupSuffix $ takeExtension "my/unknown.strange-extension"
