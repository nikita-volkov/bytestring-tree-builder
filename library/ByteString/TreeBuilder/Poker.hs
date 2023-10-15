module ByteString.TreeBuilder.Poker where

import ByteString.TreeBuilder.Prelude
import Foreign hiding (void)
import qualified Data.ByteString as A
import qualified Data.ByteString.Internal as B
import qualified Foreign as D
import qualified ByteString.TreeBuilder.Tree as E


-- |
-- Write the given bytes into the pointer and
-- return a pointer incremented by the amount of written bytes.
pokeBytes :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeBytes (B.PS foreignPointer offset length) pointer =
  do
    withForeignPtr foreignPointer $ \pointer' ->
      copyBytes pointer (plusPtr pointer' offset) length
    pure (plusPtr pointer length)

-- |
-- Write the given bytes into the pointer and
-- return a pointer decremented by the amount of written bytes.
pokeBytesMinus :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeBytesMinus (B.PS foreignPointer offset length) pointer =
  do
    withForeignPtr foreignPointer $ \pointer' ->
      copyBytes targetPointer (plusPtr pointer' offset) length
    pure targetPointer
  where
    targetPointer =
      plusPtr pointer (negate length)

pokeTree :: E.Tree -> D.Ptr Word8 -> IO (D.Ptr Word8)
pokeTree tree ptr =
  case tree of
    E.Leaf bytes -> pokeBytes bytes ptr
    E.Branch tree1 tree2 -> pokeTree tree1 ptr >>= pokeTree tree2
    E.Empty -> pure ptr
