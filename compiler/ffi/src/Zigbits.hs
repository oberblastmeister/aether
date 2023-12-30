{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Zigbits
  ( zig_intern_bytestring,
    zig_intern_resolve,
    zig_intern_compare,
  )
where

import Data.Int
import Data.Word
import Foreign
import Foreign.C
import GHC.Exts (ByteArray#)

foreign import ccall unsafe "zig_intern_bytestring" zig_intern_bytestring :: ByteArray# -> CSize -> CSize -> Word64

foreign import ccall unsafe "zig_intern_resolve" zig_intern_resolve :: Word64 -> Ptr Word8

foreign import ccall unsafe "zig_intern_compare" zig_intern_compare :: Word64 -> Word64 -> Int8
