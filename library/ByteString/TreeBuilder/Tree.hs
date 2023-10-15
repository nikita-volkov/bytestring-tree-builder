module ByteString.TreeBuilder.Tree where

import ByteString.TreeBuilder.Prelude hiding (foldl, foldlM, foldr, length)

data Tree
  = Empty
  | Leaf !ByteString
  | Branch !Tree !Tree

{-# INLINE foldl #-}
foldl :: (a -> ByteString -> a) -> a -> Tree -> a
foldl step init =
  \case
    Empty ->
      init
    Leaf value ->
      step init value
    Branch tree1 tree2 ->
      foldl step (foldl step init tree1) tree2

{-# INLINE foldr #-}
foldr :: (ByteString -> a -> a) -> a -> Tree -> a
foldr step init =
  \case
    Empty ->
      init
    Leaf value ->
      step value init
    Branch tree1 tree2 ->
      foldr step (foldr step init tree2) tree1

{-# INLINE foldlM #-}
foldlM :: (Monad m) => (a -> ByteString -> m a) -> a -> Tree -> m a
foldlM step init =
  \case
    Empty ->
      return init
    Leaf value ->
      step init value
    Branch tree1 tree2 ->
      foldlM step init tree1 >>= \init2 -> foldlM step init2 tree2
