module Data.NonDet.Map
  ( Map,
    toList,
    toListNonDet,
    fromList,
    size,
    nonDetMap,
  )
where

import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.NonDet.Class
import Optics

newtype Map k v = Map {nonDetMap :: Map.Map (NonDet k) v}

nonDetMap :: Iso (Map k v) (Map k' v') (Map.Map (NonDet k) v) (Map.Map (NonDet k') v')
nonDetMap = coerced
{-# INLINE nonDetMap #-}

toList :: forall k v. (Ord k) => Map k v -> [(k, v)]
toList = List.sortOn fst . coerce @[(NonDet k, v)] @[(k, v)] . Map.toList . (.nonDetMap)
{-# INLINE toList #-}

toListNonDet :: Map k v -> [(NonDet k, v)]
toListNonDet = Map.toList . (.nonDetMap)
{-# INLINE toListNonDet #-}

fromList :: forall k v. (NonDetOrd k) => [(k, v)] -> Map k v
fromList = Map . Map.fromList . coerce @[(k, v)] @[(NonDet k, v)]
{-# INLINE fromList #-}

size :: Map k v -> Int
size = Map.size . (.nonDetMap)
{-# INLINE size #-}

type instance IxValue (Map k v) = v

type instance Index (Map k v) = k

instance (NonDetOrd k) => Ixed (Map k v) where
  ix i = nonDetMap % ix (NonDet i)
  {-# INLINE ix #-}

instance (NonDetOrd k) => At (Map k v) where
  at i = nonDetMap % at (NonDet i)
  {-# INLINE at #-}
