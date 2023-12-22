module Data.Sym
  ( Sym,
  )
where

import Data.Intern (internText)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.ToInt (ToInt (..))
import Imports

newtype Sym = Sym {unSym :: Int64}
  deriving (Show, Eq, Generic, Hashable)

instance ToInt Sym where
  toInt (Sym i) = fromIntegral i
  {-# INLINE toInt #-}

instance IsString Sym where
  fromString = Sym . internText . T.pack
  {-# INLINE fromString #-}
