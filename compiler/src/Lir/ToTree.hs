module Lir.ToTree where

import Cfg qualified
import Imports
import Lir.Instr qualified as Lir

data Instr c
  = Instr (Lir.Instr (Instr c) c)
  | Operand Lir.Operand
  deriving (Show, Eq)

type Graph = Cfg.Graph Instr

type Block = Cfg.Block Instr

graphToTree :: Lir.Graph -> Graph
graphToTree = todo

blockToTree :: Lir.Block -> Block
blockToTree = todo
