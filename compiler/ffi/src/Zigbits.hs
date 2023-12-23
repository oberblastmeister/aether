{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Zigbits where

import GHC.Exts

foreign import ccall unsafe "zig_intern_bytestring" c_zig_intern_bytestring :: ByteArray# -> Int# -> Int# -> Word64#

foreign import ccall unsafe "zig_intern_resolve" c_zig_intern_resolve :: Word64# -> Addr#
