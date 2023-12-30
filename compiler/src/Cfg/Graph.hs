{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cfg.Graph
  ( Block (..),
    Label,
    LabelMap,
    Graph (..),
    Control (..),
    SControl (..),
    HasSControl (..),
    blockInstrsForward,
    graphPrecessors,
    traverseBlock,
    mapBlock,
    singletonGraph,
    blockInstrsBackward,
    traverseBlockOrderM,
  )
where

import Cfg.Types
import Cfg.Types qualified as Cfg
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HM
import Data.Kind qualified as Kind
import Data.List qualified as List
import Data.Some (Some (..))
import Imports

data Control = E | O | C

data SControl c where
  SE :: SControl E
  SO :: SControl O
  SC :: SControl C

class HasSControl c where
  sControl :: SControl c

instance HasSControl E where
  sControl = SE

instance HasSControl O where
  sControl = SO

instance HasSControl C where
  sControl = SC

data Block i = Block
  { entry :: i E,
    body :: [i O],
    exit :: i C
  }

type InstrConstraint (c :: Kind.Type -> Constraint) i = (c (i C), c (i O), c (i E))

deriving instance (InstrConstraint Show i) => Show (Block i)

deriving instance (InstrConstraint Eq i) => Eq (Block i)

mapBlock :: (forall c. i c -> j c) -> Block i -> Block j
mapBlock f = runIdentity . traverseBlock (pure @Identity . f)

traverseBlock :: (Applicative f) => (forall c. i c -> f (j c)) -> Block i -> f (Block j)
traverseBlock f block = do
  entry <- f block.entry
  body <- traverse f block.body
  exit <- f block.exit
  pure Block {entry, body, exit}

blockInstrsForward :: Fold (Block i) (Some i)
blockInstrsForward = foldVL go
  where
    go f block =
      f (Some (block.entry))
        *> traverse_ (f . Some) block.body
        *> f (Some block.exit)

blockInstrsBackward :: Fold (Block i) (Some i)
blockInstrsBackward = foldVL go
  where
    go f block =
      f (Some block.exit)
        *> traverse_ (f . Some) (reverse block.body)
        *> f (Some (block.entry))

type LabelMap = HashMap Label

data Graph i = Graph
  { -- invariant: the block with this label cannot be jumped to
    -- also must not have any block arguments
    start :: Label,
    blocks :: HashMap Label (Block i),
    -- currently we have only one exit label, but might need to support more (tail calls)
    -- invariant: the block with this label must have no jumps
    end :: Label
  }

instance (InstrConstraint Show i) => Show (Graph i) where
  show graph =
    "Graph { start = "
      <> show (graph.start)
      <> ", blocks = "
      <> show (List.sortBy (Cfg.compareLabel `on` fst) $ HM.toList graph.blocks)
      <> ", end = "
      <> show (graph.end)
      <> "}"

deriving instance (InstrConstraint Eq i) => Eq (Graph i)

makeFieldLabelsNoPrefix ''Graph
makeFieldLabelsNoPrefix ''Block

mapHashMapWithOrderM :: (Hashable k, Monad m) => [k] -> (k -> a -> m a) -> HashMap k a -> m (HashMap k a)
mapHashMapWithOrderM order f hm = do
  foldlM
    ( \hm k -> do
        let v = HM.lookupDefault (error "mapHashMapWithOrderM: key not found") k hm
        v' <- f k v
        pure $! HM.insert k v' hm
    )
    hm
    order

traverseBlockOrderM :: (Monad f) => (Label -> Block i -> f (Block i)) -> Graph i -> f (Graph i)
traverseBlockOrderM f graph = do
  blocks <- traverseOf (ix graph.start) (f graph.start) graph.blocks
  blocks <- mapHashMapWithOrderM (List.sort . filter (\l -> l /= graph.start && l /= graph.end) . HM.keys $ graph.blocks) (f) blocks
  blocks <- traverseOf (ix graph.end) (f graph.end) blocks
  pure graph {blocks}

singletonGraph :: Label -> Block i -> Graph i
singletonGraph label block =
  Graph
    { start = label,
      blocks = HM.singleton label block,
      end = label
    }

graphPrecessors :: (HasJumps (i C)) => Graph i -> HashMap Label [Label]
graphPrecessors graph =
  HM.fromListWith
    (++)
    [ (to, [from])
      | (from, block) <- itoListOf each graph.blocks,
        to <- jumps block.exit
    ]
