module Lir.Graph where

import Data.IdMap (IdMap)
import Imports

type family InstrType instr

type family ControlType instr

data Block instr = Block
  { body :: Vector (InstrType instr),
    end :: ControlType instr
  }

newtype Label = Label {unLabel :: Int}
  deriving (Show, Eq, Ord, Hashable)

data Graph instrs = Graph
  { start :: Label,
    labels :: IdMap Label (Block instrs),
    end :: Label
  }
