module Main where

import Test.Tasty

-- library tests
import Test.Gophermap

-- server executable tests
import Test.FileTypeDetection

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ gophermapTests
  , fileTypeDetectionTests
  ]
