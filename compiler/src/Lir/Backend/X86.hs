module Lir.Backend.X86 where

import Imports

data Instr op
  = Add op op op
  | Sub op op op
  | Mul op op op
  | Div op op op
  | Mov op op
  | Mov64 op Int64
  | Ret
  deriving (Show, Eq, Functor, Foldable, Traversable)
