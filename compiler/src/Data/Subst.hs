module Data.Subst where

import Data.HashMap.Strict qualified as HM
import Imports

newtype Subst k v = Subst {getSubst :: (HashMap k v)}

class Substitutable a where
  apply :: Subst k v -> a -> a

singleton :: (Hashable k) => k -> v -> Subst k v
singleton k v = Subst (HM.singleton k v)

fromList :: (Hashable k) => [(k, v)] -> Subst k v
fromList = Subst . HM.fromList

compose :: Subst k v -> Subst k v -> Subst k v
compose (Subst s1) (Subst s2) = (\v -> v) <$> s2
