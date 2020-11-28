module Main where

import BasePrelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ByteString.TreeBuilder
import qualified Data.ByteString.Lazy


main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    testProperty "Concatting a list of bytestrings is isomorphic to appending a list of builders (strict)" $
    \bytestrings ->
      mconcat bytestrings ===
      toByteString (foldMap byteString bytestrings)
    ,
    testProperty "Concatting a list of bytestrings is isomorphic to appending a list of builders (lazy)" $
    \bytestrings ->
      mconcat bytestrings ===
      Data.ByteString.Lazy.toStrict (toLazyByteString (foldMap byteString bytestrings))
    ,
    testCase "Regression (Issue #5)" $
    let
      fn name =
        toByteString $
          "FETCH FORWARD "
            <> asciiIntegral 10
            <> " FROM "
            <> byteString name
      in assertEqual ""
        "FETCH FORWARD 10 FROM abc"
        (fn "abc")
  ]
