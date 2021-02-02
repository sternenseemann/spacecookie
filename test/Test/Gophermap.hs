{-# LANGUAGE OverloadedStrings #-}
module Test.Gophermap (gophermapTests) where

import Control.Monad (forM_)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as B
import Data.Either
import Network.Gopher (GopherFileType (..))
import Network.Gopher.Util (stripNewline)
import Network.Gopher.Util.Gophermap
import System.FilePath.Posix.ByteString (RawFilePath)
import Test.Tasty
import Test.Tasty.HUnit

withFileContents :: FilePath -> (IO B.ByteString -> TestTree) -> TestTree
withFileContents path = withResource (B.readFile path) (const (pure ()))

gophermapTests :: TestTree
gophermapTests = testGroup "gophermap tests"
  [ withFileContents "test/data/pygopherd.gophermap" checkPygopherd
  , withFileContents "test/data/bucktooth.gophermap" checkBucktooth
  , generalGophermapParsing
  ]

checkPygopherd :: IO B.ByteString -> TestTree
checkPygopherd file = testCase "pygopherd example gophermap" $
  file >>= assertEqual "" (Right expectedPygopherd) . parseOnly parseGophermap

infoLine :: B.ByteString -> GophermapEntry
infoLine b = GophermapEntry InfoLine b Nothing Nothing Nothing

absDir :: B.ByteString -> RawFilePath -> B.ByteString -> GophermapEntry
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

generalGophermapParsing :: TestTree
generalGophermapParsing = testGroup "gophermap entry test cases" $
  let lineEqual :: B.ByteString -> GophermapEntry -> Assertion
      lineEqual b e = assertEqual (show b) (Right [e]) $
        parseOnly parseGophermap b
      infoLines =
        [ "1. beginning with valid file type\n"
        , "just some usual text.\n"
        , "ends with end of input"
        , "i'm blue"
        , "0"
        , "empty ones need to be terminated by a new line\n"
        , "\n"
        , "otherwise parsing doesn't make sense anymore"
        , "DOS-style newlines are also allowed\r\n"
        ]
      menuEntry t name path =
        GophermapEntry t name (Just path) Nothing Nothing
      menuLines =
        [ ("1/somedir\t", GophermapEntry Directory "/somedir" Nothing Nothing Nothing)
        , ("0file\tfile.txt\n", menuEntry File "file" (GophermapRelative "file.txt"))
        , ("ggif\t/pic.gif", menuEntry GifFile "gif" (GophermapAbsolute "/pic.gif"))
        , ("hcode\tURL:https://code.sterni.lv\n", menuEntry Html "code" (GophermapUrl "URL:https://code.sterni.lv"))
        , ("1foo\tfoo\tsterni.lv", GophermapEntry Directory "foo" (Just $ GophermapRelative "foo") (Just "sterni.lv") Nothing)
        , ("Ibar\t/bar.png\tsterni.lv\t7070\n", GophermapEntry ImageFile "bar" (Just $ GophermapAbsolute "/bar.png") (Just "sterni.lv") (Just 7070))
        , ("imanual info line\t", infoLine "manual info line")
        ]
   in [ testCase "info lines" $ forM_ infoLines (\l -> lineEqual l $ infoLine (stripNewline l))
      , testCase "menu entries" $ forM_ menuLines (uncurry lineEqual) ]
