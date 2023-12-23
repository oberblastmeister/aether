{-# LANGUAGE MagicHash #-}

module Data.Intern
  ( internText,
    internByteArray,
    unsafeResolvePtr,
    unsafeResolveByteString,
  )
where

import Data.Bits qualified as Bits
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive
import Data.TextUtils qualified as TextUtils
import GHC.ForeignPtr qualified as ForeignPtr
import GHC.Int (Int (..))
import GHC.Word (Word64 (..))
import Imports
import Zigbits

internText :: Text -> Word64
internText (TextUtils.UnsafeTextSlice bs off len) = internByteArray bs off len
{-# INLINE internText #-}

internByteArray :: ByteArray -> Int -> Int -> Word64
internByteArray (ByteArray bs#) (I# off#) (I# len#) = W64# (c_zig_intern_bytestring bs# off# len#)
{-# INLINE internByteArray #-}

unsafeResolvePtr :: Word64 -> Ptr Word8
unsafeResolvePtr (W64# i#) = Ptr (c_zig_intern_resolve i#)
{-# INLINE unsafeResolvePtr #-}

unsafeResolveByteString :: Word64 -> ByteString
unsafeResolveByteString w@(W64# i#) =
  B.Internal.fromForeignPtr (ForeignPtr.ForeignPtr (c_zig_intern_resolve i#) ForeignPtr.FinalPtr) 0 len
  where
    len :: Int
    len = fromIntegral (w `Bits.unsafeShiftR` 44)
{-# INLINE unsafeResolveByteString #-}
