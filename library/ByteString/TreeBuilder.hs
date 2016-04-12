module ByteString.TreeBuilder
(
  Builder,
  -- * Declaration
  -- ** Primitives
  byteString,
  byte,
  -- ** Extras
  asciiIntegral,
  asciiChar,
  utf8Char,
  utf8Ord,
  utf8Text,
  utf8LazyText,
  -- * Execution
  length,
  toByteString,
  toLazyByteString,
)
where

import ByteString.TreeBuilder.Prelude hiding (foldl, foldr, length)
import qualified ByteString.TreeBuilder.Tree as A
import qualified ByteString.TreeBuilder.Poker as D
import qualified ByteString.TreeBuilder.Prelude as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy.Internal as E
import qualified Data.Text
import qualified Data.Text.Lazy


-- |
-- A binary-tree-based datastructure optimized for aggregation of bytestrings
-- using the /O(1)/ appending operation.
data Builder =
  Builder !Int !A.Tree

-- |
-- Implements `mappend` with /O(1)/ complexity.
instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder 0 A.Empty
  {-# INLINABLE mappend #-}
  mappend (Builder length1 tree1) (Builder length2 tree2) =
    Builder (length1 + length2) (A.Branch tree1 tree2)
  {-# INLINE mconcat #-}
  mconcat =
    foldl' mappend mempty

instance Semigroup Builder where
  {-# INLINE sconcat #-}
  sconcat =
    foldl' mappend mempty

instance IsString Builder where
  {-# INLINE fromString #-}
  fromString string =
    Builder (B.length bytes) (A.Leaf bytes)
    where
      bytes =
        fromString string


-- * Declaration
-------------------------

-- ** Primitives
-------------------------

-- |
-- Lifts a bytestring into the builder.
{-# INLINE byteString #-}
byteString :: ByteString -> Builder
byteString bytes =
  Builder (B.length bytes) (A.Leaf bytes)

-- |
-- Lifts a single byte into the builder.
{-# INLINE byte #-}
byte :: Word8 -> Builder
byte byte =
  Builder 1 (A.Leaf (B.singleton byte))

-- ** Extras
-------------------------

{-# INLINABLE asciiIntegral #-}
asciiIntegral :: Integral a => a -> Builder
asciiIntegral =
  \case
    0 ->
      byte 48
    x ->
      bool ((<>) (byte 45)) id (x >= 0) $
      loop mempty $
      abs x
  where
    loop builder remainder =
      case remainder of
        0 ->
          builder
        _ ->
          case quotRem remainder 10 of
            (quot, rem) ->
              loop (byte (48 + fromIntegral rem) <> builder) quot

{-# INLINE asciiChar #-}
asciiChar :: Char -> Builder
asciiChar =
  byte . fromIntegral . ord

{-# INLINE utf8Char #-}
utf8Char :: Char -> Builder
utf8Char =
  utf8Ord . ord

{-# INLINE utf8Ord #-}
utf8Ord :: Int -> Builder
utf8Ord x =
  if x <= 0x7F
    then
      byte (fromIntegral x)
    else 
      if x <= 0x07FF
        then
          byte (fromIntegral ((x `shiftR` 6) + 0xC0)) <>
          byte (fromIntegral ((x .&. 0x3F) + 0x80))
        else
          if x <= 0xFFFF
            then
              byte (fromIntegral (x `shiftR` 12) + 0xE0) <>
              byte (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80) <>
              byte (fromIntegral (x .&. 0x3F) + 0x80)
            else
              byte (fromIntegral (x `shiftR` 18) + 0xF0) <>
              byte (fromIntegral ((x `shiftR` 12) .&. 0x3F) + 0x80) <>
              byte (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80) <>
              byte (fromIntegral (x .&. 0x3F) + 0x80)

{-# INLINE utf8Text #-}
utf8Text :: Data.Text.Text -> Builder
utf8Text =
  Data.Text.foldl' (\builder -> mappend builder . utf8Char) mempty

{-# INLINE utf8LazyText #-}
utf8LazyText :: Data.Text.Lazy.Text -> Builder
utf8LazyText =
  Data.Text.Lazy.foldl' (\builder -> mappend builder . utf8Char) mempty


-- * Execution
-------------------------

-- |
-- Performs a left-fold over the aggregated chunks.
{-# INLINE foldl #-}
foldl :: (a -> ByteString -> a) -> a -> Builder -> a
foldl step init (Builder length tree) =
  A.foldl step init tree

-- |
-- Performs a right-fold over the aggregated chunks.
{-# INLINE foldr #-}
foldr :: (ByteString -> a -> a) -> a -> Builder -> a
foldr step init (Builder length tree) =
  A.foldr step init tree

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
  foldr E.Chunk E.Empty
