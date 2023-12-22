module Data.TextUtils
  ( copyTextBytes,
    unsafeEncodeByteArrayUtf8,
    pattern TextUtf8,
  )
where

import Data.Primitive.ByteArray
import Data.Text.Internal qualified as T.Internal
import Imports

copyTextBytes :: Text -> ByteArray
copyTextBytes (T.Internal.Text bs offset len) = runByteArray $ do
  mba <- newByteArray len
  copyByteArray mba 0 bs offset len
  pure mba
{-# INLINE copyTextBytes #-}

pattern TextUtf8 :: ByteArray -> Int -> Int -> Text
pattern TextUtf8 bs offset len <- T.Internal.Text bs (offset) (len)

{-# COMPLETE TextUtf8 #-}

unsafeEncodeByteArrayUtf8 :: ByteArray -> Text
unsafeEncodeByteArrayUtf8 bs = T.Internal.Text bs 0 (sizeofByteArray bs)
{-# INLINE unsafeEncodeByteArrayUtf8 #-}
