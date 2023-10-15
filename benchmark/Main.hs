module Main where

import BasePrelude
import qualified ByteString.TreeBuilder as A
import Criterion.Main
import Data.ByteString (ByteString)

main :: IO ()
main =
  defaultMain $
    map sampleGroup $
      [("Small Input", smallSample), ("Medium Input", mediumSample), ("Large Input", largeSample)]

sampleGroup :: (String, Sample) -> Benchmark
sampleGroup (title, (Sample sample)) =
  bench title $
    nf sample $
      (A.byteString, mappend, mempty, A.toByteString)

newtype Sample
  = Sample (forall a. (ByteString -> a, a -> a -> a, a, a -> ByteString) -> ByteString)

{-# NOINLINE smallSample #-}
smallSample :: Sample
smallSample =
  Sample $
    \(fromByteString, (<>), mempty, toByteString) ->
      toByteString $
        (fromByteString "hello" <> fromByteString "asdf")
          <> fromByteString "fsndfn"
          <> (fromByteString "dfgknfg" <> fromByteString "aaaaaa")

{-# NOINLINE mediumSample #-}
mediumSample :: Sample
mediumSample =
  Sample $
    \(fromByteString, (<>), mempty, toByteString) ->
      toByteString $
        foldl' (<>) mempty $
          replicate 1000 $
            (fromByteString "hello" <> fromByteString "asdf")
              <> fromByteString "fsndfn"
              <> (fromByteString "dfgknfg" <> fromByteString "aaaaaa")

{-# NOINLINE largeSample #-}
largeSample :: Sample
largeSample =
  Sample $
    \(fromByteString, (<>), mempty, toByteString) ->
      toByteString $
        foldl' (<>) mempty $
          replicate 100000 $
            (fromByteString "hello" <> fromByteString "asdf")
              <> fromByteString "fsndfn"
              <> (fromByteString "dfgknfg" <> fromByteString "aaaaaa")
