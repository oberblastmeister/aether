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
    blockInstructions,
    graphPrecessors,
    traverseBlock,
    mapBlock,
  )
where

import Cfg.Types
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HM
import Data.Some (Some (..))
import Imports

data Control = E | O | C

data Block i = Block
  { entry :: i E,
    body :: [i O],
    exit :: i C
  }

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

makeFieldLabelsNoPrefix ''Graph
makeFieldLabelsNoPrefix ''Block

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

