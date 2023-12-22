module Lir.Instr where

import Imports

data InstrKind op
  = Add op op
  | Sub op op
  | Mul op op
  | Div op op
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Operand
  = Const Int64
  | Var Name

data OperandPat
  = ConstPat Name
  | VarPat Name
  | OperandPat Name

type Name = Text

data Instr op = Instr {mut :: Bool, res :: (Maybe Name), instr :: (InstrKind op)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

pattern (:=) :: Name -> (InstrKind op) -> Instr op
pattern name := instr = Instr False (Just name) instr

pattern (::=) :: Name -> (InstrKind op) -> Instr op
pattern name ::= instr = Instr True (Just name) instr
