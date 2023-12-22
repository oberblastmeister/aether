{-# LANGUAGE DefaultSignatures #-}

module Data.BitSet
  ( BitSet,
    Key (..),
    contains,
    insert,
    remove,
    iso,
  )
where

import Data.Bits ((!<<.), (!>>.))
import Data.Bits qualified as Bits
import Data.ToInt
import Imports hiding (Contains (..), iso)
import Optics qualified as O

newtype BitSet a = BitSet {unBitSet :: Word64}
  deriving (Show, Eq, Ord, Generic, Hashable)

instance Semigroup (BitSet a) where
  BitSet a <> BitSet b = BitSet (a .|. b)
  {-# INLINE (<>) #-}

instance Monoid (BitSet a) where
  mempty = BitSet 0
  {-# INLINE mempty #-}

class Key a where
  fromKey :: a -> Int
  default fromKey :: (ToInt a) => a -> Int
  fromKey k = if i >= 64 then error "fromKey: index too large" else i
    where
      i = toInt k
  {-# INLINE fromKey #-}

type instance Index (BitSet a) = a

type instance IxValue (BitSet a) = Bool

iso :: Iso' (BitSet a) Word64
iso = O.iso (.unBitSet) BitSet
{-# INLINE iso #-}

contains :: (Key a) => a -> BitSet a -> Bool
contains k (BitSet b) = b !>>. fromKey k .&. 1 == 1
{-# INLINE contains #-}

insert :: (Key a) => a -> BitSet a -> BitSet a
insert k (BitSet b) = BitSet (b .|. (1 !<<. fromKey k))
{-# INLINE insert #-}

remove :: (Key a) => a -> BitSet a -> BitSet a
remove i (BitSet b) = BitSet (b .&. Bits.complement (1 !<<. fromKey i))
{-# INLINE remove #-}

type instance Index (BitSet a) = a

instance (Key a) => O.Contains (BitSet a) where
  contains i =
    lens
      (contains i)
      (\bs x -> if x then insert i bs else remove i bs)
  {-# INLINE contains #-}
