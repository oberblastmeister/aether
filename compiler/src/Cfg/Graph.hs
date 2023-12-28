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
    blockInstructions,
    graphPrecessors,
    traverseBlock,
    mapBlock,
    singletonGraph,
  )
where

import Cfg.Types
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HM
import Data.Kind qualified as Kind
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

blockInstructions :: Fold (Block i) (Some i)
blockInstructions = foldVL go
  where
    go f block = f (Some (block.entry)) *> traverse_ (f . Some) block.body *> f (Some block.exit)

type LabelMap = HashMap Label

data Graph i = Graph
  { start :: Label,
    blocks :: HashMap Label (Block i),
    end :: Label
  }

deriving instance (InstrConstraint Show i) => Show (Graph i)

deriving instance (InstrConstraint Eq i) => Eq (Graph i)

makeFieldLabelsNoPrefix ''Graph
makeFieldLabelsNoPrefix ''Block

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

data Testing = Testing {_thingy :: Int, bruh :: Int}

makeLensesWith underscoreFields ''Testing
