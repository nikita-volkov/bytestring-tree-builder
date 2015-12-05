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
import qualified ChunkTree as A
import qualified ByteString.TreeBuilder.Poker as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy.Internal as E
import qualified ByteString.TreeBuilder.Prelude as F


-- |
-- A binary-tree-based datastructure optimized for aggregation of bytestrings.
-- Implements the appending operation with /O(1)/ complexity.
newtype Builder =
  Builder (A.ChunkTree ByteString)
  deriving (Monoid)

instance IsString Builder where
  {-# INLINE fromString #-}
  fromString string =
    Builder (A.chunk (B.length bytes) bytes)
    where
      bytes =
        fromString string

-- |
-- Lifts a bytestring into the builder.
{-# INLINE byteString #-}
byteString :: ByteString -> Builder
byteString bytes =
  Builder (A.chunk (B.length bytes) bytes)

-- |
-- Lifts a single bytestring into the builder.
{-# INLINE byte #-}
byte :: Word8 -> Builder
byte byte =
  Builder (A.chunk 1 (B.singleton byte))

-- * Execution
-------------------------

-- |
-- Performs a left-fold over the chunks of which the builder consists.
{-# INLINE foldl #-}
foldl :: (a -> ByteString -> a) -> a -> Builder -> a
foldl step init (Builder chunkTree) =
  A.foldl step init chunkTree

-- |
-- /O(1)/. Gets the total length.
{-# INLINE length #-}
length :: Builder -> Int
length (Builder chunkTree) =
  A.length chunkTree

-- |
-- /O(n)/. Converts the builder into a strict bytestring.
{-# INLINABLE toByteString #-}
toByteString :: Builder -> ByteString
toByteString (Builder chunkTree) =
  C.unsafeCreate (A.length chunkTree) $ \ptr -> 
    void $ A.foldlM (flip D.pokeBytes) ptr chunkTree

-- |
-- /O(n)/. Converts the builder into a lazy bytestring.
{-# INLINABLE toLazyByteString #-}
toLazyByteString :: Builder -> E.ByteString
toLazyByteString =
  foldl (flip E.Chunk) E.Empty
