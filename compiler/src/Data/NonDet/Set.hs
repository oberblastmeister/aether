module Data.NonDet.Set
  ( Set (..),
    fromList,
    difference,
    union,
    intersection,
    unions,
    size,
    empty,
    toListNonDet,
    toListOrd,
    insert,
  )
where

import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.NonDet.Class
import Data.Set qualified as Set

newtype Set a = Set {getSet :: Set.Set (NonDet a)}
  deriving (Show, Eq, Ord, Hashable, NFData, Semigroup, Monoid)

fromList :: (NonDetOrd a) => [a] -> Set a
fromList = Set . Set.fromList . coerce
{-# INLINE fromList #-}

toListOrd :: (Ord a) => Set a -> [a]
toListOrd = List.sort . coerce (Set.toList :: Set.Set (NonDet a) -> [NonDet a])
{-# INLINE toListOrd #-}

toListNonDet :: Set a -> [NonDet a]
toListNonDet = coerce (Set.toList :: Set.Set (NonDet a) -> [NonDet a])
{-# INLINE toListNonDet #-}

difference :: forall a. (NonDetOrd a) => Set a -> Set a -> Set a
difference = coerce (Set.difference :: Set.Set (NonDet a) -> Set.Set (NonDet a) -> Set.Set (NonDet a))
{-# INLINE difference #-}

insert :: forall a. (NonDetOrd a) => a -> Set a -> Set a
insert = coerce (Set.insert :: NonDet a -> Set.Set (NonDet a) -> Set.Set (NonDet a))
{-# INLINE insert #-}

union :: forall a. (NonDetOrd a) => Set a -> Set a -> Set a
union = coerce (Set.union :: Set.Set (NonDet a) -> Set.Set (NonDet a) -> Set.Set (NonDet a))
{-# INLINE union #-}

unions :: forall a. (NonDetOrd a) => [Set a] -> Set a
unions = coerce (Set.unions :: [Set.Set (NonDet a)] -> Set.Set (NonDet a))
{-# INLINE unions #-}

empty :: Set a
empty = Set Set.empty
{-# INLINE empty #-}

size :: Set a -> Int
size = Set.size . (.getSet)
{-# INLINE size #-}

intersection :: forall a. (NonDetOrd a) => Set a -> Set a -> Set a
intersection = coerce (Set.intersection :: Set.Set (NonDet a) -> Set.Set (NonDet a) -> Set.Set (NonDet a))
{-# INLINE intersection #-}
