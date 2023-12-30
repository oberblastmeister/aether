{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Str
  ( Str,
    getText,
    compareContents,
  )
where

import Control.DeepSeq (NFData (..))
import Data.ByteString.Short qualified as BS
import Data.Intern
import Data.NonDet.Class (NonDetOrd (..))
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.TextUtils qualified as TextUtils
import Data.ToInt (ToInt (..))
import Data.Word
import GHC.Records (HasField (..))
import Imports

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

newtype Str = UnsafeStr Word64

instance NFData Str where
  rnf (UnsafeStr w) = w `seq` ()
  {-# INLINE rnf #-}

instance NonDetOrd Str where
  nonDetCompare (UnsafeStr w) (UnsafeStr w') = compare w w'
  {-# INLINE nonDetCompare #-}

instance Ord Str where
  compare (UnsafeStr s1) (UnsafeStr s2) =
    if s1 == s2
      then EQ
      else
        compare (getSymbolLength s1) (getSymbolLength s2)
          <> unsafeCompareSymbol s1 s2
  {-# INLINE compare #-}

compareContents :: Str -> Str -> Ordering
compareContents (UnsafeStr s1) (UnsafeStr s2) = unsafeCompareSymbol s1 s2
{-# INLINE compareContents #-}

instance Show Str where
  show = show . getText
  {-# INLINE show #-}

instance Eq Str where
  (UnsafeStr w) == (UnsafeStr w') = w == w'
  {-# INLINE (==) #-}

instance ToInt Str where
  toInt (UnsafeStr w) = fromIntegral w
  {-# INLINE toInt #-}

instance Hashable Str where
  hashWithSalt salt (UnsafeStr w) = hashWithSalt salt w
  {-# INLINE hashWithSalt #-}

instance IsString Str where
  fromString s = UnsafeStr (unsafeInternByteArray bs off len)
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
toBS (UnsafeStr w) = unsafeResolveByteString w
{-# INLINE toBS #-}
