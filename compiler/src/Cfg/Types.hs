{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cfg.Types
  ( Name (..),
    HasJumps (..),
    HasUses (..),
    HasDefs (..),
    Label (..),
    nameStr,
    compareName,
    nameFromText,
  )
where

import Data.Str (NonDetStr (..))
import Data.String (IsString (..))
import Imports

data Name
  = StrName NonDetStr
  | GenName NonDetStr Int
  deriving (Show, Eq, Ord, Generic)

instance IsString Name where
  fromString = nameFromText . fromString

nameFromText :: Text -> Name
nameFromText = StrName . NonDetStr . (.str)

compareName :: Name -> Name -> Ordering
compareName = compare `on` by
  where
    by (StrName str) = Left str.getStr
    by (GenName str i) = Right (str.getStr, i)

nameStr :: Name -> NonDetStr
nameStr (StrName str) = str
nameStr (GenName str _) = str

instance Hashable Name

class HasDefs a n | a -> n where
  defs :: a -> [n]

class HasUses a n | a -> n where
  uses :: a -> [n]

newtype Label = Label {name :: Name}
  deriving (Show, Eq, Hashable, IsString)

class HasJumps a where
  jumps :: a -> [Label]
