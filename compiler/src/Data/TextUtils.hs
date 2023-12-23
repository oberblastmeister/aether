{-# LANGUAGE MagicHash #-}

module Data.TextUtils
  ( copyTextBytes,
    pattern UnsafeTextSlice,
    unsafeText,
    pattern Short,
    unShort,
  )
where

import Data.ByteString.Short (ShortByteString (..))
import Data.Primitive.ByteArray
import Data.Text.Internal qualified as T.Internal
import Imports

copyTextBytes :: Text -> ByteArray
copyTextBytes (T.Internal.Text bs offset len) = runByteArray $ do
  mba <- newByteArray len
  copyByteArray mba 0 bs offset len
  pure mba
{-# INLINE copyTextBytes #-}

unsafeText :: ByteArray -> Text
unsafeText bs = T.Internal.Text bs 0 (sizeofByteArray bs)
{-# INLINE unsafeText #-}

pattern UnsafeTextSlice :: ByteArray -> Int -> Int -> Text
pattern UnsafeTextSlice bs off len = T.Internal.Text bs off len
{-# INLINE UnsafeTextSlice #-}

{-# COMPLETE UnsafeTextSlice #-}

pattern Short :: ByteArray -> ShortByteString
pattern Short bs <- (SBS (ByteArray -> bs))
  where
    Short (ByteArray bs#) = SBS bs#

{-# COMPLETE Short #-}

{-# INLINE Short #-}

unShort :: ShortByteString -> ByteArray
unShort (Short bs) = bs
{-# INLINE unShort #-}
