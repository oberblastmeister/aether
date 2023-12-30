{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module Data.NonDet.Class
  ( NonDetOrd (..),
    NonDet (..),
    OrdToNonDet (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics qualified as G

class (Eq a) => NonDetOrd a where
  nonDetCompare :: a -> a -> Ordering
  default nonDetCompare :: (G.Generic a, GNonDetOrd (G.Rep a)) => a -> a -> Ordering
  nonDetCompare a b = gNonDetCompare (G.from a) (G.from b)
  {-# INLINE nonDetCompare #-}

class GNonDetOrd f where
  gNonDetCompare :: f a -> f a -> Ordering

instance GNonDetOrd G.V1 where
  gNonDetCompare _ _ = EQ
  {-# INLINE gNonDetCompare #-}

instance GNonDetOrd G.U1 where
  gNonDetCompare _ _ = EQ
  {-# INLINE gNonDetCompare #-}

instance (NonDetOrd a) => GNonDetOrd (G.K1 i a) where
  gNonDetCompare (G.K1 a) (G.K1 b) = nonDetCompare a b
  {-# INLINE gNonDetCompare #-}

instance (GNonDetOrd a) => GNonDetOrd (G.M1 i c a) where
  gNonDetCompare (G.M1 a) (G.M1 b) = gNonDetCompare a b
  {-# INLINE gNonDetCompare #-}

instance (GNonDetOrd a, GNonDetOrd b) => GNonDetOrd (a G.:*: b) where
  gNonDetCompare (a G.:*: b) (a' G.:*: b') = gNonDetCompare a a' <> gNonDetCompare b b'
  {-# INLINE gNonDetCompare #-}

instance (GNonDetOrd a, GNonDetOrd b) => GNonDetOrd (a G.:+: b) where
  gNonDetCompare (G.L1 a) (G.L1 b) = gNonDetCompare a b
  gNonDetCompare (G.R1 a) (G.R1 b) = gNonDetCompare a b
  gNonDetCompare (G.L1 _) (G.R1 _) = LT
  gNonDetCompare (G.R1 _) (G.L1 _) = GT
  {-# INLINE gNonDetCompare #-}

instance (GNonDetOrd a) => GNonDetOrd (G.Rec1 a) where
  gNonDetCompare (G.Rec1 a) (G.Rec1 b) = gNonDetCompare a b
  {-# INLINE gNonDetCompare #-}

instance NonDetOrd () where
  nonDetCompare _ _ = EQ
  {-# INLINE nonDetCompare #-}

instance NonDetOrd Bool where
  nonDetCompare = compare
  {-# INLINE nonDetCompare #-}

instance NonDetOrd Char where
  nonDetCompare = compare
  {-# INLINE nonDetCompare #-}

instance NonDetOrd Int where
  nonDetCompare = compare
  {-# INLINE nonDetCompare #-}

instance NonDetOrd Text where
  nonDetCompare = compare
  {-# INLINE nonDetCompare #-}

instance (NonDetOrd a, NonDetOrd b) => NonDetOrd (Either a b)

instance (NonDetOrd a) => NonDetOrd (Maybe a)

instance (NonDetOrd a) => NonDetOrd [a]

instance (NonDetOrd a, NonDetOrd b) => NonDetOrd (a, b)

instance (NonDetOrd a, NonDetOrd b, NonDetOrd c) => NonDetOrd (a, b, c)

newtype OrdToNonDet a = LiftOrd {value :: a}
  deriving (Show, Eq, Hashable, Functor, Foldable, Traversable, G.Generic, NFData)

instance (Ord a) => NonDetOrd (OrdToNonDet a) where
  nonDetCompare = compare `on` (.value)
  {-# INLINE nonDetCompare #-}

newtype NonDet a = NonDet {getNonDet :: a}
  deriving (Show, Eq, Hashable, Functor, Foldable, Traversable, G.Generic, NFData)

instance (NonDetOrd a) => Ord (NonDet a) where
  compare = nonDetCompare `on` (.getNonDet)
  {-# INLINE compare #-}
