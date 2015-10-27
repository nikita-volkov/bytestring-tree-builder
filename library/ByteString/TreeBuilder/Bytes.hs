module ByteString.TreeBuilder.Bytes where

import ByteString.TreeBuilder.Prelude
import Foreign hiding (void)
import qualified Data.ByteString as A
import qualified Data.ByteString.Internal as B
import qualified ByteString.TreeBuilder.BinaryTree as C
import qualified Foreign as D


-- |
-- Write the given bytes into the pointer and
-- return a pointer incremented by the amount of written bytes.
{-# INLINE pokeBytes #-}
pokeBytes :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeBytes (B.PS foreignPointer offset length) pointer =
  do
    withForeignPtr foreignPointer $ \pointer' ->
      B.memcpy pointer (plusPtr pointer' offset) length
    pure (plusPtr pointer length)

-- |
-- Write the given bytes into the pointer and
-- return a pointer decremented by the amount of written bytes.
{-# INLINE pokeBytesMinus #-}
pokeBytesMinus :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeBytesMinus (B.PS foreignPointer offset length) pointer =
  do
    withForeignPtr foreignPointer $ \pointer' ->
      B.memcpy targetPointer (plusPtr pointer' offset) length
    pure targetPointer
  where
    targetPointer =
      plusPtr pointer (negate length)

{-# INLINE pokeTree #-}
pokeTree :: C.BinaryTree ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeTree tree ptr =
  case tree of
    C.Leaf bytes -> pokeBytes bytes ptr
    C.Branch tree1 tree2 -> pokeTree tree1 ptr >>= pokeTree tree2
