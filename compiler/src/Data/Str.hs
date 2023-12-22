{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Str
  ( Str,
    getText,
  )
where

import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import Data.Intern
import Data.Primitive
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.ToInt (ToInt (..))
import GHC.Records (HasField (..))

instance HasField "str" Text Str where
  getField = fromString . T.unpack
  {-# INLINE getField #-}

instance HasField "str" String Str where
  getField = fromString
  {-# INLINE getField #-}

instance HasField "t" Str Text where
  getField = getText
  {-# INLINE getField #-}

instance HasField "s" Str String where
  getField = T.unpack . getText
  {-# INLINE getField #-}

data Str = Str ByteArray Int64

instance Show Str where
  show (Str bs _) = show (TextUtils.unsafeEncodeByteArrayUtf8 bs)
  {-# INLINE show #-}

instance Eq Str where
  Str _t1 i1 == Str _t2 i2 = i1 == i2
  {-# INLINE (==) #-}

instance ToInt Str where
  toInt (Str _t i) = fromIntegral i
  {-# INLINE toInt #-}

instance Hashable Str where
  hashWithSalt salt (Str _t i) = hashWithSalt salt i
  {-# INLINE hashWithSalt #-}

instance IsString Str where
  fromString s = Str bs (internByteArray bs 0 (sizeofByteArray bs))
    where
      bs = TextUtils.copyTextBytes (T.pack s)
  {-# INLINE fromString #-}

getText :: Str -> Text
getText (Str bs _) = TextUtils.unsafeEncodeByteArrayUtf8 bs
{-# INLINE getText #-}
