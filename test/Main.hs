module Main where

import Test.Tasty

import Test.Gophermap

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ gophermapTests ]
