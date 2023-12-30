{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cfg.Types
  ( Name (..),
    HasJumps (..),
    HasUses (..),
    HasDefs (..),
    Label (..),
    nameFromText,
    compareLabel,
    showName,
  )
where

import Data.NonDet.Class (NonDetOrd)
import Data.Str (Str)
import Data.String (IsString (..))
import Imports

data Name
  = StrName {str :: Str}
  | GenName {str :: Str, gen :: Int}
  deriving (Show, Eq, Ord, Generic)

instance NonDetOrd Name

instance IsString Name where
  fromString = nameFromText . fromString

showName :: Name -> Text
showName (StrName s) = s.t
showName (GenName s i) = s.t <> ".".t <> (show i).t

nameFromText :: Text -> Name
nameFromText = StrName . (.str)

instance Hashable Name

class HasDefs a n | a -> n where
  defs :: a -> [n]

class HasUses a n | a -> n where
  uses :: a -> [n]

newtype Label = Label {name :: Name}
  deriving (Show, Eq, Ord, NonDetOrd, Hashable, IsString)

compareLabel :: Label -> Label -> Ordering
compareLabel = compare `on` (.name.str.t)

class HasJumps a where
  jumps :: a -> [Label]
