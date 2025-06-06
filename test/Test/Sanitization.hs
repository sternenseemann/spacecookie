{-# LANGUAGE OverloadedStrings #-}
module Test.Sanitization (sanitizationTests) where

import Network.Spacecookie.FileType (checkNoDotFiles, PathError (..))
import Network.Spacecookie.Path (sanitizePath, makeAbsolute)

import Control.Monad (forM_)
import qualified Data.ByteString.UTF8 as UTF8
import System.FilePath.Posix.ByteString (isAbsolute)
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
  let assertSanitize e p = assertEqual p e $ sanitizePath (UTF8.fromString p)
  assertSanitize "/root" "/root"
  assertSanitize "/home/alice/.emacs.d/init.el" "/home/alice/.emacs.d/init.el"

  assertSanitize "root" "./root"
  assertSanitize"/tools/magrathea" "//tools/magrathea"
  assertSanitize "/home/bob/Documents/important.txt" "/home/bob//Documents/important.txt"

  assertSanitize "foo/bar/baz.txt" "./foo/bar/./baz.txt"
  assertSanitize "/var/www/index..html" "/var/www/.///index..html"
  assertSanitize "./" "./."
  assertSanitize "/" "/."
  assertSanitize "home/eve/" "./home/./././eve////./."

  assertSanitize  "/home/bob/alice/private.txt" "/home/bob/../alice/private.txt"

dotFileDetectionTest :: TestTree
dotFileDetectionTest = testCase "spacecookie server detects dot files in paths" $ do
  let assertDot p hasDot = forM_
        [ (p, UTF8.fromString p)
        , (p ++ " (sanitized)", sanitizePath (UTF8.fromString p))
        ]
        $ \(title, path) -> assertEqual title
          (if hasDot then Left PathIsNotAllowed else Right ())
          $ checkNoDotFiles path

  assertDot "./normal/relative/path" False
  assertDot "." False
  assertDot "/some/absolute/path" False
  assertDot "file.txt" False
  assertDot "/foo.html" False
  assertDot "./tmp/scratch.txt" False
  assertDot "./." False
  assertDot  "relative/./path" False

  assertDot ".emacs.d/init.el" True
  assertDot ".gophermap" True
  assertDot "/home/bob/.vimrc" True
  assertDot "/home/alice/.config/foot" True
  assertDot "./nixpkgs/.git/config" True

  -- only fail prior to sanitization
  forM_
    [ "dir/../traversal/../attack", "../../../actual/traversal" ]
    $ \p -> do
        let p' = UTF8.fromString p
        assertEqual p (Left PathIsNotAllowed) $ checkNoDotFiles p'
        assertEqual p (Right ()) $ checkNoDotFiles (sanitizePath p')

makeAbsoluteTest :: TestTree
makeAbsoluteTest = testCase "relative paths are correctly converted to absolute ones" $ do
  let assertAbsolute expected given = do
        assertEqual given expected $ makeAbsolute (UTF8.fromString given)
        assertBool ("makeAbsolute " ++ given ++ " is absolute") $ isAbsolute (makeAbsolute (UTF8.fromString given))

  assertAbsolute "/foo/bar" "/foo/bar"
  assertAbsolute "/foo/bar" "./foo/bar"
  assertAbsolute "/foo/bar" "foo/bar"
  assertAbsolute "/" "."
  assertAbsolute "/" "./"
  assertAbsolute "/bar/foo" "././bar/foo"
  assertAbsolute "/../bar/foo" "./../bar/foo"
  assertAbsolute "/../bar/foo" "../bar/foo"
