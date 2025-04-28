module Main where

import Test.Tasty

-- library tests
import Test.Gophermap
-- library-ish
import Test.Sanitization

-- server executable tests
import Test.FileTypeDetection
import Test.Integration

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ gophermapTests
  , sanitizationTests
  , fileTypeDetectionTests
  , integrationTests
  ]
