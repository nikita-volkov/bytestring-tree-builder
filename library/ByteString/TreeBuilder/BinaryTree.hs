module ByteString.TreeBuilder.BinaryTree where

import ByteString.TreeBuilder.Prelude hiding (foldl, foldlM, foldr)


data BinaryTree a =
  Leaf !a |
  Branch !(BinaryTree a) !(BinaryTree a)
  deriving (Functor, Foldable, Traversable)

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> BinaryTree b -> a
foldl step init =
  \case
    Leaf value ->
      step init value
    Branch tree1 tree2 ->
      foldl step (foldl step init tree1) tree2

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> BinaryTree b -> a
foldr step init =
  \case
    Leaf value ->
      step value init
    Branch tree1 tree2 ->
      foldr step (foldr step init tree2) tree1

{-# INLINE foldlM #-}
foldlM :: Monad m => (a -> b -> m a) -> a -> BinaryTree b -> m a
foldlM step init =
  \case
    Leaf value ->
      step init value
    Branch tree1 tree2 ->
      foldlM step init tree1 >>= \init2 -> foldlM step init2 tree2
