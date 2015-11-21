module Main where

import BasePrelude
import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import qualified ByteString.TreeBuilder as A


main =
  defaultMain $
  map sampleGroup $
  [("Small Input", smallSample), ("Medium Input", mediumSample), ("Large Input", largeSample)]
    
sampleGroup :: (String, Sample) -> Benchmark
sampleGroup (title, sample) =
  bench title $ nf sample $
  (A.byteString, mappend, mempty, A.toByteString)

type Sample =
  forall a. (ByteString -> a, a -> a -> a, a, a -> ByteString) -> ByteString

{-# NOINLINE smallSample #-}
smallSample :: Sample
smallSample (fromByteString, (<>), mempty, toByteString) =
  toByteString $
    (fromByteString "hello" <> fromByteString "asdf") <>
    fromByteString "fsndfn" <>
    (fromByteString "dfgknfg" <> fromByteString "aaaaaa")

{-# NOINLINE mediumSample #-}
mediumSample :: Sample
mediumSample (fromByteString, (<>), mempty, toByteString) =
  toByteString $
  foldl' (<>) mempty $ replicate 1000 $
    (fromByteString "hello" <> fromByteString "asdf") <>
    fromByteString "fsndfn" <>
    (fromByteString "dfgknfg" <> fromByteString "aaaaaa")

{-# NOINLINE largeSample #-}
largeSample :: Sample
largeSample (fromByteString, (<>), mempty, toByteString) =
  toByteString $
  foldl' (<>) mempty $ replicate 100000 $
    (fromByteString "hello" <> fromByteString "asdf") <>
    fromByteString "fsndfn" <>
    (fromByteString "dfgknfg" <> fromByteString "aaaaaa")

