{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.IdMap
  ( IdMap (..),
    fromList,
    fromListCoerce,
    difference,
    intersection,
    union,
    mapIso,
    ToInt (..),
  )
where

import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict qualified as IntMap
import Data.ToInt
import Imports

type role IdMap nominal representational

newtype IdMap k v = IdMap {unEnumMap :: IntMap v}
  deriving (Show, Eq, Ord, Generic, Hashable, Functor, Foldable, Traversable)

mapIso :: Iso' (IdMap k v) (IntMap v)
mapIso = coerced
{-# INLINE mapIso #-}

type instance Index (IdMap k v) = k

type instance IxValue (IdMap k v) = v

instance (ToInt k) => Ixed (IdMap k v) where
  ix i = mapIso % ix (toInt i)
  {-# INLINE ix #-}

instance (ToInt k) => At (IdMap k v) where
  at i = mapIso % at (toInt i)
  {-# INLINE at #-}
  
-- instance FoldableWithIndex k (IdMap k) where
--   ifoldMap f = ifoldMap f . unEnumMap
--   {-# INLINE ifoldMap #-}

fromList :: forall k v. (ToInt k) => [(k, v)] -> IdMap k v
fromList = coerce (IntMap.fromList :: [(Int, v)] -> IntMap v) . map (first toInt)
{-# INLINE fromList #-}

fromListCoerce :: forall k v. (Coercible k Int) => [(k, v)] -> IdMap k v
fromListCoerce = coerce (IntMap.fromList :: [(Int, v)] -> IntMap v)
{-# INLINE fromListCoerce #-}

difference :: forall k a b. IdMap k a -> IdMap k b -> IdMap k a
difference = coerce (IntMap.difference :: IntMap a -> IntMap b -> IntMap a)
{-# INLINE difference #-}

intersection :: forall k a b. IdMap k a -> IdMap k b -> IdMap k a
intersection = coerce (IntMap.intersection :: IntMap a -> IntMap b -> IntMap a)
{-# INLINE intersection #-}

union :: forall k a. IdMap k a -> IdMap k a -> IdMap k a
union = coerce (IntMap.union :: IntMap a -> IntMap a -> IntMap a)
{-# INLINE union #-}
