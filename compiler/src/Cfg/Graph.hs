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
  )
where

import Cfg.Types (Name)
import Data.Some (Some (..))
import Imports

data Control = E | O | C

data Block i = Block
  { entry :: i E,
    body :: [i O],
    exit :: i C
  }

blockInstructions :: Fold (Block i) (Some i)
blockInstructions = foldVL go
  where
    go f block = f (Some (block.entry)) *> traverse_ (f . Some) block.body *> f (Some block.exit)

type Label = Name

type LabelMap = HashMap Label

data Graph i = Graph
  { start :: Label,
    blocks :: HashMap Label (Block i),
    end :: Label
  }

makeFieldLabelsNoPrefix ''Block
makeFieldLabelsNoPrefix ''Graph
