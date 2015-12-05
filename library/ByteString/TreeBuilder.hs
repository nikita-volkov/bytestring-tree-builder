module ByteString.TreeBuilder
(
  Builder,
  -- * Declaration
  byteString,
  byte,
  -- * Execution
  toByteString,
  toLazyByteString,
)
where

import ByteString.TreeBuilder.Prelude hiding (foldl, length)
import qualified ByteString.TreeBuilder.Tree as A
import qualified ByteString.TreeBuilder.Poker as D
import qualified ByteString.TreeBuilder.Prelude as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy.Internal as E


-- |
-- A binary-tree-based datastructure optimized for aggregation of bytestrings.
-- Implements the appending operation with /O(1)/ complexity.
data Builder =
  Builder !Int !A.Tree

-- |
-- Implements an /O(1)/ `mappend`.
instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder 0 A.Empty
  {-# INLINABLE mappend #-}
  mappend (Builder length1 tree1) (Builder length2 tree2) =
    Builder (length1 + length2) (A.Branch tree1 tree2)

instance IsString Builder where
  {-# INLINE fromString #-}
  fromString string =
    Builder (B.length bytes) (A.Leaf bytes)
    where
      bytes =
        fromString string

-- |
-- Lifts a bytestring into the builder.
{-# INLINE byteString #-}
byteString :: ByteString -> Builder
byteString bytes =
  Builder (B.length bytes) (A.Leaf bytes)

-- |
-- Lifts a single bytestring into the builder.
{-# INLINE byte #-}
byte :: Word8 -> Builder
byte byte =
  Builder 1 (A.Leaf (B.singleton byte))

-- * Execution
-------------------------

-- |
-- Performs a left-fold over the chunks of which the builder consists.
{-# INLINE foldl #-}
foldl :: (a -> ByteString -> a) -> a -> Builder -> a
foldl step init (Builder length tree) =
  A.foldl step init tree

-- |
-- /O(1)/. Gets the total length.
{-# INLINE length #-}
length :: Builder -> Int
length (Builder length tree) =
  length

-- |
-- /O(n)/. Converts the builder into a strict bytestring.
{-# INLINABLE toByteString #-}
toByteString :: Builder -> ByteString
toByteString (Builder length tree) =
  C.unsafeCreate length $ \ptr -> 
    void $ D.pokeTree tree ptr

-- |
-- /O(n)/. Converts the builder into a lazy bytestring.
{-# INLINABLE toLazyByteString #-}
toLazyByteString :: Builder -> E.ByteString
toLazyByteString =
  foldl (flip E.Chunk) E.Empty
