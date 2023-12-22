{-# LANGUAGE MagicHash #-}

module Data.Intern
  ( internText,
    internByteArray,
  )
where

import Data.Primitive
import Data.Text.Internal qualified as T.Internal
import GHC.Int (Int (..), Int64 (..))
import Imports
import Zigbits

internText :: Text -> Int64
internText (T.Internal.Text bs offset len) = internByteArray bs offset len
{-# INLINE internText #-}

internByteArray :: ByteArray -> Int -> Int -> Int64
internByteArray (ByteArray bs#) (I# off#) (I# len#) = I64# (c_zig_intern_bytestring bs# off# len#)
{-# INLINE internByteArray #-}
