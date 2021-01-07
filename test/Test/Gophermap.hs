{-# LANGUAGE OverloadedStrings #-}
module Test.Gophermap (gophermapTests) where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as B
import Data.Either
import Network.Gopher (GopherFileType (..))
import Network.Gopher.Util.Gophermap
import Test.Tasty
import Test.Tasty.HUnit

withFileContents :: FilePath -> (IO B.ByteString -> TestTree) -> TestTree
withFileContents path = withResource (B.readFile path) (const (pure ()))

gophermapTests :: TestTree
gophermapTests = testGroup "gophermap tests"
  [ withFileContents "test/data/pygopherd.gophermap" checkPygopherd
  , withFileContents "test/data/bucktooth.gophermap" checkBucktooth
  ]

checkPygopherd :: IO B.ByteString -> TestTree
checkPygopherd file = testCase "pygopherd example gophermap" $
  file >>= assertEqual "" (Right expectedPygopherd) . parseOnly parseGophermap

infoLine :: B.ByteString -> GophermapEntry
infoLine b = GophermapEntry InfoLine b Nothing Nothing Nothing

absDir :: B.ByteString -> FilePath -> B.ByteString -> GophermapEntry
absDir n p s =
  GophermapEntry Directory n (Just (GophermapAbsolute p)) (Just s) $ Just 70

expectedPygopherd :: Gophermap
expectedPygopherd =
  [ infoLine "Welcome to Pygopherd!  You can place your documents"
  , infoLine "in /var/gopher for future use.  You can remove the gophermap"
  , infoLine "file there to get rid of this message, or you can edit it to"
  , infoLine "use other things.  (You'll need to do at least one of these"
  , infoLine "two things in order to get your own data to show up!)"
  , infoLine ""
  , infoLine "Some links to get you started:"
  , infoLine ""
  , absDir "Pygopherd Home" "/devel/gopher/pygopherd" "gopher.quux.org"
  , absDir "Quux.Org Mega Server" "/" "gopher.quux.org"
  , absDir "The Gopher Project" "/Software/Gopher" "gopher.quux.org"
  , absDir "Traditional UMN Home Gopher" "/" "gopher.tc.umn.edu"
  , infoLine ""
  , infoLine "Welcome to the world of Gopher and enjoy!"
  ]

checkBucktooth :: IO B.ByteString -> TestTree
checkBucktooth file = testCase "bucktooth example gophermap" $ do
  parseResult <- parseOnly parseGophermap <$> file

  assertBool "no parse failure" $ isRight parseResult

  -- check if we can distinguish between text/infolines and
  -- gophermap lines which have no path
  assertEqual "overbite link is parsed correctly" [expectedOverbiteEntry]
    . filter (\(GophermapEntry _ n _ _ _) -> n == "/overbite")
    $ fromRight [] parseResult

  assertEqual "correct length" 95 . length $ fromRight [] parseResult

expectedOverbiteEntry :: GophermapEntry
expectedOverbiteEntry =
  GophermapEntry Directory "/overbite" Nothing Nothing Nothing
