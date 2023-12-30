{-# LANGUAGE MagicHash #-}

module Data.Intern
  ( internText,
    unsafeInternByteArray,
    unsafeResolvePtr,
    unsafeResolveByteString,
    unsafeCompareSymbol,
    getSymbolLength,
  )
where

import Data.Bits qualified as Bits
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive
import Data.TextUtils qualified as TextUtils
import Foreign.C (CSize)
import GHC.Exts (Addr#)
import GHC.ForeignPtr qualified as ForeignPtr
import Imports
import Zigbits

internText :: Text -> Word64
internText (TextUtils.UnsafeTextSlice bs off len) = unsafeInternByteArray bs off len
{-# INLINE internText #-}

unsafeCompareSymbol :: Word64 -> Word64 -> Ordering
unsafeCompareSymbol s1 s2 =
  case zig_intern_compare s1 s2 of
    0 -> EQ
    -1 -> LT
    _ -> GT
{-# INLINE unsafeCompareSymbol #-}

unsafeInternByteArray :: ByteArray -> Int -> Int -> Word64
unsafeInternByteArray (ByteArray bs#) off len = zig_intern_bytestring bs# (intToCSize off) (intToCSize len)
{-# INLINE unsafeInternByteArray #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

unsafeResolvePtr :: Word64 -> Ptr Word8
unsafeResolvePtr s = zig_intern_resolve s
{-# INLINE unsafeResolvePtr #-}

unsafeResolveByteString :: Word64 -> ByteString
unsafeResolveByteString s =
  B.Internal.fromForeignPtr
    (ForeignPtr.ForeignPtr (ptrAddr# (zig_intern_resolve s)) ForeignPtr.FinalPtr)
    0
    (getSymbolLength s)
{-# INLINE unsafeResolveByteString #-}

ptrAddr# :: Ptr a -> Addr#
ptrAddr# (Ptr addr#) = addr#
{-# INLINE ptrAddr# #-}

getSymbolLength :: Word64 -> Int
getSymbolLength w = fromIntegral (w `Bits.unsafeShiftR` 44)
{-# INLINE getSymbolLength #-}
