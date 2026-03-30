{-# LANGUAGE QuasiQuotes #-}
module Test.Sanitization (sanitizationTests) where

import Network.Spacecookie.FileType (checkNoDotFiles, PathError (..))
import Network.Spacecookie.Path (sanitizePath, makeAbsolute)

import Control.Monad (forM_)
import System.OsPath.Posix (isAbsolute)
import System.OsString.Posix (pstr)
import Test.Tasty
import Test.Tasty.HUnit

sanitizationTests :: TestTree
sanitizationTests = testGroup "Sanitization of user input"
 [ pathSanitization
 , dotFileDetectionTest
 , makeAbsoluteTest
 ]

pathSanitization :: TestTree
pathSanitization = testCase "sanitizePath behavior" $ do
  let assertSanitize e p = assertEqual (show p) e $ sanitizePath p
  assertSanitize [pstr|/root|] [pstr|/root|]
  assertSanitize [pstr|/home/alice/.emacs.d/init.el|] [pstr|/home/alice/.emacs.d/init.el|]

  assertSanitize [pstr|root|] [pstr|./root|]
  assertSanitize [pstr|/tools/magrathea|] [pstr|//tools/magrathea|]
  assertSanitize [pstr|/home/bob/Documents/important.txt|] [pstr|/home/bob//Documents/important.txt|]

  assertSanitize [pstr|foo/bar/baz.txt|] [pstr|./foo/bar/./baz.txt|]
  assertSanitize [pstr|/var/www/index..html|] [pstr|/var/www/.///index..html|]
  assertSanitize [pstr|./|] [pstr|./.|]
  assertSanitize [pstr|/|] [pstr|/.|]
  assertSanitize [pstr|home/eve/|] [pstr|./home/./././eve////./.|]

  assertSanitize  [pstr|/home/bob/alice/private.txt|] [pstr|/home/bob/../alice/private.txt|]

dotFileDetectionTest :: TestTree
dotFileDetectionTest = testCase "spacecookie server detects dot files in paths" $ do
  let assertDot p hasDot = forM_
        [ (show p, p)
        , (show p ++ " (sanitized)", sanitizePath p)
        ]
        $ \(title, path) -> assertEqual title
          (if hasDot then Left PathIsNotAllowed else Right ())
          $ checkNoDotFiles path

  assertDot [pstr|./normal/relative/path|] False
  assertDot [pstr|.|] False
  assertDot [pstr|/some/absolute/path|] False
  assertDot [pstr|file.txt|] False
  assertDot [pstr|/foo.html|] False
  assertDot [pstr|./tmp/scratch.txt|] False
  assertDot [pstr|./.|] False
  assertDot [pstr|relative/./path|] False

  assertDot [pstr|.emacs.d/init.el|] True
  assertDot [pstr|.gophermap|] True
  assertDot [pstr|/home/bob/.vimrc|] True
  assertDot [pstr|/home/alice/.config/foot|] True
  assertDot [pstr|./nixpkgs/.git/config|] True

  -- only fail prior to sanitization
  forM_
    [ [pstr|dir/../traversal/../attack|], [pstr|../../../actual/traversal|] ]
    $ \p -> do
        assertEqual (show p) (Left PathIsNotAllowed) $ checkNoDotFiles p
        assertEqual (show p) (Right ()) $ checkNoDotFiles (sanitizePath p)

makeAbsoluteTest :: TestTree
makeAbsoluteTest = testCase "relative paths are correctly converted to absolute ones" $ do
  let assertAbsolute expected given = do
        assertEqual (show given) expected $ makeAbsolute given
        assertBool ("makeAbsolute " ++ show given ++ " is absolute")
          $ isAbsolute (makeAbsolute given)

  assertAbsolute [pstr|/foo/bar|] [pstr|/foo/bar|]
  assertAbsolute [pstr|/foo/bar|] [pstr|./foo/bar|]
  assertAbsolute [pstr|/foo/bar|] [pstr|foo/bar|]
  assertAbsolute [pstr|/|] [pstr|.|]
  assertAbsolute [pstr|/|] [pstr|./|]
  assertAbsolute [pstr|/bar/foo|] [pstr|././bar/foo|]
  assertAbsolute [pstr|/../bar/foo|] [pstr|./../bar/foo|]
  assertAbsolute [pstr|/../bar/foo|] [pstr|../bar/foo|]
