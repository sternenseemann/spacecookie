{-# LANGUAGE OverloadedStrings #-}
module Test.Sanitization (sanitizationTests) where

import Network.Gopher.Util (uEncode)
import Network.Spacecookie.FileType (checkNoDotFiles, PathError (..))
import Network.Spacecookie.Path (sanitizePath)

import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit

sanitizationTests :: TestTree
sanitizationTests = testGroup "Sanitization of user input"
 [ pathSanitization
 , dotFileDetectionTest
 ]

pathSanitization :: TestTree
pathSanitization = testCase "sanitizePath behavior" $ do
  let assertSanitize e p = assertEqual p e $ sanitizePath (uEncode p)
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
        [ (p, uEncode p)
        , (p ++ " (sanitized)", sanitizePath (uEncode p))
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

  assertDot ".emacs.d/init.el" True
  assertDot ".gophermap" True
  assertDot "/home/bob/.vimrc" True
  assertDot "/home/alice/.config/foot" True
  assertDot "./nixpkgs/.git/config" True

  -- only fail prior to sanitization
  forM_
    [ "./.", "relative/./path", "dir/../traversal/../attack", "../../../actual/traversal" ]
    $ \p -> do
        let p' = uEncode p
        assertEqual p (Left PathIsNotAllowed) $ checkNoDotFiles p'
        assertEqual p (Right ()) $ checkNoDotFiles (sanitizePath p')
