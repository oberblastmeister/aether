{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Zigbits where

import GHC.Exts

foreign import ccall unsafe "zig_intern_bytestring" c_zig_intern_bytestring :: ByteArray# -> Int# -> Int# -> Int64#
