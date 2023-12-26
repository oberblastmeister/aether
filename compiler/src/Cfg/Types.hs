{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cfg.Types
  ( Name (..),
    HasJumps (..),
    HasUses (..),
    HasDefs (..),
    nameStr,
  )
where

import Data.Str (Str)
import Imports

data Name
  = StrName Str
  | GenName Str Int
  deriving (Show, Eq, Ord, Generic)

nameStr :: Name -> Str
nameStr (StrName str) = str
nameStr (GenName str _) = str

instance Hashable Name

class HasDefs a n | a -> n where
  defs :: a -> [n]

class HasUses a n | a -> n where
  uses :: a -> [n]

class HasJumps a where
  jumps :: a -> [Name]
