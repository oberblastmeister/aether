{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cfg.Types
  ( Name (..),
    HasJumps (..),
    HasUses (..),
    HasDefs (..),
    Label (..),
    nameStr,
    compareName,
  )
where

import Data.Str (NonDetStr (..), Str)
import Imports

data Name
  = StrName NonDetStr
  | GenName NonDetStr Int
  deriving (Show, Eq, Ord, Generic)

compareName :: Name -> Name -> Ordering
compareName = compare `on` by
  where
    by (StrName str) = Left str.getStr
    by (GenName str i) = Right (str.getStr, i)

-- compare (StrName str) (StrName str') = compare str str'
-- compare (GenName _ i) (GenName _ i') = compare i i'
-- compare (StrName _) (GenName _ _) = LT
-- compare (GenName _ _) (StrName _) = GT

nameStr :: Name -> NonDetStr
nameStr (StrName str) = str
nameStr (GenName str _) = str

instance Hashable Name

class HasDefs a n | a -> n where
  defs :: a -> [n]

class HasUses a n | a -> n where
  uses :: a -> [n]

newtype Label = Label {name :: Name}
  deriving (Show, Eq, Hashable)

class HasJumps a where
  jumps :: a -> [Label]
