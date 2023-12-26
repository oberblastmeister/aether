{-# LANGUAGE UndecidableInstances #-}

module Lir.Instr
  ( OpInstr (..),
    InstrControl (..),
    BlockCall (..),
    Operand (..),
    Instr (..),
    Graph,
    Block,
    pattern (:=),
    instrOperands,
    operandNames,
    runLiveness,
    blockCalls,
    defsTraversal,
    usesTraversal,
  )
where

import Cfg (Control (..))
import Cfg qualified
import Data.Kind (Type)
import Data.Str (Str)
import Imports

type Graph = Cfg.Graph (Instr Operand)

type Block = Cfg.Block (Instr Operand)

data OpInstr op
  = Add op op
  | Sub op op
  | Mul op op
  | Div op op
  | Call Str [op]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data InstrControl op
  = Jump (BlockCall op)
  | CondJump op (BlockCall op) (BlockCall op)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data BlockCall op = BlockCall
  { label :: Cfg.Name,
    args :: [op]
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Operand
  = Const Int64
  | Var Cfg.Name
  deriving (Show, Eq)

data Instr :: Type -> Control -> Type where
  BlockArgs :: [Cfg.Name] -> Instr op E
  Assign :: Cfg.Name -> OpInstr op -> Instr op O
  Control :: InstrControl op -> Instr op C

deriving instance (Show op) => Show (Instr op c)

deriving instance (Eq op) => Eq (Instr op c)

pattern (:=) :: Cfg.Name -> (OpInstr op) -> Instr op O
pattern name := instr = Assign name instr

{-# COMPLETE (:=) #-}

blockCalls :: Traversal' (Instr op C) (BlockCall op)
blockCalls = traversalVL $ \f (Control instr) ->
  Control <$> case instr of
    Jump bc -> Jump <$> f bc
    CondJump op bc1 bc2 -> CondJump op <$> f bc1 <*> f bc2

instrOperands :: Traversal (Instr op c) (Instr op' c) op op'
instrOperands = traversalVL $ \f -> \case
  BlockArgs names -> pure $ BlockArgs names
  Assign name instr -> Assign name <$> traverse f instr
  Control instr -> Control <$> traverse f instr

operandNames :: Traversal' Operand Cfg.Name
operandNames = traversalVL $ \f -> \case
  Var name -> Var <$> f name
  Const n -> pure (Const n)

defsTraversal :: Traversal' (Instr op c) Cfg.Name
defsTraversal = traversalVL $ \f -> \case
  BlockArgs names -> BlockArgs <$> traverse f names
  Assign name instr -> Assign <$> f name <*> pure instr
  other -> pure other

usesTraversal :: Traversal' (Instr Operand c) Cfg.Name
usesTraversal = instrOperands % operandNames

instance Cfg.HasDefs (Instr c op) Cfg.Name where
  defs = (^.. defsTraversal)

instance Cfg.HasUses (Instr Operand c) Cfg.Name where
  uses = (^.. usesTraversal)

instance Cfg.HasJumps (Instr op C) where
  jumps = (^.. blockCalls % to (.label))

runLiveness :: Graph -> Cfg.LabelMap (Set Cfg.Name)
runLiveness = Cfg.runTransfer Cfg.livenessTransfer
