{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module Data.BigGraph
  ( BigGraph,
    addEdge,
  )
where

import Data.HashMap.Strict qualified as HM
import Imports

newtype BigGraph k w v = BigGraph
  { unBigGraph :: (HashMap k (Context k w v))
  }

data Context k w v = Context
  { pred :: HashMap k w,
    value :: v,
    succ :: HashMap k w
  }

emptyContext :: v -> Context k w v
emptyContext v = Context HM.empty v HM.empty

_unBigGraph :: Lens (BigGraph k w v) (BigGraph k' w' v') (HashMap k (Context k w v)) (HashMap k' (Context k' w' v'))
_unBigGraph = lens (.unBigGraph) (\_ g -> BigGraph g)

_value :: Lens (Context k a v) (Context k a v') v v'
_value = lens (.value) (\(Context pred _ succ) v -> Context pred v succ)

_pred :: Lens (Context k a v) (Context k a v) (HashMap k a) (HashMap k a)
_pred = lens (.pred) (\(Context _ v succ) p -> Context p v succ)

_succ :: Lens (Context k a v) (Context k a v) (HashMap k a) (HashMap k a)
_succ = lens (.succ) (\(Context pred v _) s -> Context pred v s)

data Vertex k v = Vertex
  { key :: k,
    value :: v
  }
  deriving (Show, Eq)

type instance Index (BigGraph k w v) = k

type instance IxValue (BigGraph k w v) = v

instance (Hashable k, Eq k) => Ixed (BigGraph k w v) where
  ix i = _unBigGraph % ix i % _value
  {-# INLINE ix #-}

instance (Hashable k, Eq k) => At (BigGraph k w v) where
  at i = undefined
    where
      o = _unBigGraph % at i % at @(Maybe _) ()

-- at i = lens (HM.lookup i . unBigGraph) (\g v -> g & _unBigGraph % at i .~ v)
-- {-# INLINE at #-}

--   {-# INLINE at #-}
addVertex :: (Hashable k) => Vertex k v -> BigGraph k w v -> BigGraph k w v
addVertex v g = g & _unBigGraph % at v.key %~ \case Nothing -> Just (emptyContext v.value); m -> m
{-# INLINEABLE addVertex #-}

-- | add a directed edge
addEdge :: (Hashable k) => Vertex k v -> w -> Vertex k v -> BigGraph k w v -> BigGraph k w v
addEdge v1 weight v2 g =
  g
    & _unBigGraph
    % at v1.key
    % unwrapOr (emptyContext v1.value)
    % _succ
    % at v2.key
    .~ Just weight
    & _unBigGraph
    % at v2.key
    % unwrapOr (emptyContext v2.value)
    % _pred
    % at v1.key
    .~ Just weight
{-# INLINEABLE addEdge #-}
