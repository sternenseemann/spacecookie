module Test.Sanitization (sanitizationTests) where

import Network.Gopher.Util (uEncode, sanitizePath)
import Network.Spacecookie.FileType (checkNoDotFiles, PathError (..))

import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit

sanitizationTests :: TestTree
sanitizationTests = testGroup "Sanitization of user input"
 [ dotFileDetectionTest
 ]

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

  -- unexpected cases
  assertDot "path/../traversal/../attack" True
  -- only fail prior to sanitization
  forM_
    [ "./.", "relative/./path" ]
    $ \p -> do
        let p' = uEncode p
        assertEqual p (Left PathIsNotAllowed) $ checkNoDotFiles p'
        assertEqual p (Right ()) $ checkNoDotFiles (sanitizePath p')
