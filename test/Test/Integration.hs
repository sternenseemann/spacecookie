{-# LANGUAGE OverloadedStrings #-}
module Test.Integration where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.List
import Data.Maybe (isNothing, isJust, fromJust)
import Network.Curl.Download (openURI)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (spawnProcess, terminateProcess, waitForProcess)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Providers (testPassed)
import Test.Tasty.Runners (Result (..))

spacecookieBin :: IO (Maybe FilePath)
spacecookieBin = do
  fromEnv <- lookupEnv "SPACECOOKIE_TEST_BIN"
  fromPath <- findExecutable "spacecookie"
  pure $ fromEnv <|> fromPath

ignoreTestIf :: IO Bool -> String -> TestTree -> TestTree
ignoreTestIf doSkip msg tree = wrapTest change tree
  where change normal = do
          skip <- doSkip
          if not skip
            then normal
            else pure $ (testPassed msg) {
              resultShortDescription = "SKIP"
            }

integrationTests :: TestTree
integrationTests = testGroup "integration tests"
  [ ignoreTestIf (isNothing <$> spacecookieBin) "no spacecookie executable"
      $ testCaseSteps "spacecookie server behaves as expected" integrationAsserts
  ]

integrationAsserts :: (String -> IO ()) -> Assertion
integrationAsserts step = do
  step "getting spacecookie executable"
  bin <- spacecookieBin
  assertBool "have spacecookie executable" $ isJust bin
  step "starting spacecookie executable"
  bracket (spawn (fromJust bin)) assertSuccess
    $ const $ do
      threadDelay 1000000 -- wait 1 sec for the server to start up
      step "request root menu"

      assertEqual "root menu as expected" (Right expectedRoot)
        =<< openURI "gopher://localhost:7000/0"

      assertEqual "root menu requested with / as expected" (Right expectedRoot)
        =<< openURI "gopher://localhost:7000/0/"

      step "request plain.txt"

      fileDisk <- Right <$> B.readFile "test/integration/root/plain.txt"
      fileGopher <- openURI "gopher://localhost:7000/1/plain.txt"

      assertEqual "served file is same as on disk" fileDisk fileGopher

      step "check automatically generated directory menus"

      dir <- openURI "gopher://localhost:7000/0/dir"
      dirNoSlash <- openURI "gopher://localhost:7000/0dir"

      assertEqual "directory menu is equal regardless of request" dir dirNoSlash

      -- ignore ordering for the purpose of this test
      assertEqual "directory menu contains expected entries" (Right expectedDir)
        $ sort . filter (not . B.null) . B.split (fromIntegral $ ord '\n') <$> dir

      step "sanity check not found error messages"

      notFoundError <- openURI "gopher://localhost:7000/1/does/not/exist"
      anotherNotFoundError <- openURI "gopher://localhost:7000/0/not-here.txt"
      urlNotFoundError <- openURI "gopher://localhost:7000/0URL:http://sterni.lv"

      assertEqual "precise error message"
        (Right expectedErrorMessage) anotherNotFoundError

      forM_ [ notFoundError, anotherNotFoundError, urlNotFoundError ]
        $ assertIsError

      assertBool "error responses differ for different files"
        $ notFoundError /= anotherNotFoundError

      assertBool "error response for URL: selectors is helpful"
        $ Right True == fmap (B.isInfixOf "support") urlNotFoundError

      step "sanity check not allowed error messages"

      -- can't test directory traversal since curl won't try it
      accessGophermap <- openURI "gopher://localhost:7000/0/.gophermap"
      accessNonExistentDot <- openURI "gopher://localhost:7000/0/dir/.not-here"

      forM_ [ accessGophermap, accessNonExistentDot ] $ \err -> do
        assertIsError err
        assertBool "error response is not allowed response"
          $ Right True == fmap (B.isInfixOf "allow") err

  where spawn bin = spawnProcess bin [ "test/integration/spacecookie.json" ]
        assertSuccess hdl = do
          step "stopping spacecookie"
          terminateProcess hdl
          assertEqual "spacecookie's exit code indicates SIGTERM" (ExitFailure (-15))
            =<< waitForProcess hdl
        assertIsError e = assertEqual "error response starts with a 3" (Right "3")
          $ fmap (B.take 1) e

expectedRoot :: ByteString
expectedRoot = mconcat
  [ "iHello World!\tHello World!\tlocalhost\t7000\r\n"
  , "i\t\tlocalhost\t7000\r\n"
  , "0normal text file\t/plain.txt\tlocalhost\t7000\r\n"
  , "1normal dir\t/dir\tlocalhost\t7000\r\n"
  , "i\t\tlocalhost\t7000\r\n"
  , "1external\t/\tsterni.lv\t7000\r\n"
  ]

expectedDir :: [ByteString]
expectedDir = sort
  [ "1another\t/dir/another\tlocalhost\t7000\r"
  , "0mystery-file\t/dir/mystery-file\tlocalhost\t7000\r"
  , "0strange.tXT\t/dir/strange.tXT\tlocalhost\t7000\r"
  , "4macintosh.hqx\t/dir/macintosh.hqx\tlocalhost\t7000\r"
  ]

expectedErrorMessage :: ByteString
expectedErrorMessage = mconcat
  [ "3The requested resource '/not-here.txt' does not exist"
  , " or is not available.\tErr\tlocalhost\t7000\r\n" ]
