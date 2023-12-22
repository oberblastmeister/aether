{-# LANGUAGE DefaultSignatures #-}

module Data.ToInt
  ( ToInt (..),
  )
where

import Data.Coerce (Coercible, coerce)

class ToInt a where
  toInt :: a -> Int
  default toInt :: (Coercible a Int) => a -> Int
  toInt = coerce
  {-# INLINE toInt #-}
