module Main where

import BasePrelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ByteString.TreeBuilder


main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    testProperty "Concatting a list of bytestrings is isomorphic to appending a list of builders" $
    \bytestrings ->
      mconcat bytestrings ==
      toByteString (foldMap byteString bytestrings)
  ]
