{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Str
  ( Str,
    getText,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as BS
import Data.Hashable (Hashable (..))
import Data.Intern
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.ToInt (ToInt (..))
import Data.Word
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

newtype Str = Str Word64

instance Ord Str where
  compare (Str w) (Str w') = compare w w'
  {-# INLINE compare #-}

instance Show Str where
  show = show . getText
  {-# INLINE show #-}

instance Eq Str where
  (Str w) == (Str w') = w == w'
  {-# INLINE (==) #-}

instance ToInt Str where
  toInt (Str w) = fromIntegral w
  {-# INLINE toInt #-}

instance Hashable Str where
  hashWithSalt salt (Str w) = hashWithSalt salt w
  {-# INLINE hashWithSalt #-}

instance IsString Str where
  fromString s = Str (internByteArray bs off len)
    where
      (TextUtils.UnsafeTextSlice bs off len) = T.pack s
  {-# INLINE fromString #-}

getText :: Str -> Text
getText = TextUtils.unsafeText . TextUtils.unShort . toShort
{-# INLINE getText #-}

toShort :: Str -> BS.ShortByteString
toShort = BS.toShort . toBS
{-# INLINE toShort #-}

toBS :: Str -> ByteString
toBS (Str w) = unsafeResolveByteString w
{-# INLINE toBS #-}
