module ByteString.TreeBuilder
(
  Builder,
  -- * Declaration
  byteString,
  byte,
  -- * Execution
  length,
  toByteString,
  toLazyByteString,
)
where

import ByteString.TreeBuilder.Prelude hiding (foldl, length)
import qualified ByteString.TreeBuilder.BinaryTree as A
import qualified ByteString.TreeBuilder.Bytes as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy.Internal as E


data Builder =
  Empty |
  Tree !Int {-# UNPACK #-} !(A.BinaryTree ByteString)

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Empty
  {-# INLINE mappend #-}
  mappend =
    append

{-# INLINABLE append #-}
append :: Builder -> Builder -> Builder
append =
  \case
    Empty ->
      id
    Tree length1 tree1 ->
      \case
        Empty ->
          Tree length1 tree1
        Tree length2 tree2 ->
          Tree (length1 + length2) (A.Branch tree1 tree2)

{-# INLINE byteString #-}
byteString :: ByteString -> Builder
byteString bytes =
  Tree (B.length bytes) (A.Leaf bytes)

{-# INLINE byte #-}
byte :: Word8 -> Builder
byte byte =
  Tree 1 (A.Leaf (B.singleton byte))

-- * Execution
-------------------------

{-# INLINABLE foldl #-}
foldl :: (a -> ByteString -> a) -> a -> Builder -> a
foldl step init =
  \case
    Empty ->
      init
    Tree _ tree ->
      A.foldl step init tree

{-# INLINE length #-}
length :: Builder -> Int
length =
  \case
    Empty ->
      0
    Tree length _ ->
      length

{-# INLINABLE toByteString #-}
toByteString :: Builder -> ByteString
toByteString =
  \case
    Empty ->
      B.empty
    Tree length tree ->
      C.unsafeCreate length $ \ptr -> void $ A.foldlM (flip D.pokeBytes) ptr tree

{-# INLINABLE toLazyByteString #-}
toLazyByteString :: Builder -> E.ByteString
toLazyByteString =
  foldl (flip E.Chunk) E.Empty
