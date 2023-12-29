{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lir.Instr
  ( InstrOp (..),
    InstrControl (..),
    BlockCall (..),
    Value (..),
    CmpOp (..),
    Instr (..),
    Graph,
    Block,
    Ty (..),
    pattern (:=),
    instrUses,
    runLiveness,
    blockCalls,
    instrDefs,
    _BlockArgs,
    SomeInstr (..),
    Function (..),
    compareValue,
    getOpInstrTy,
    opInstrUses,
    controlInstrUses,
  )
where

import Cfg (Control (..))
import Cfg qualified
import Data.Kind (Type)
import Data.Str (Str)
import Imports

data Function v = Function
  { name :: Str,
    params :: [Value],
    returnTy :: Ty,
    graph :: Cfg.Graph (Instr v)
  }
  deriving (Show, Eq)

type Graph = Cfg.Graph (Instr Value)

type Block = Cfg.Block (Instr Value)

data Ty = TyU1 | TyU64
  deriving (Show, Eq)

data CmpOp
  = CmpGt
  deriving (Show, Eq)

data InstrOp v
  = Add Ty v v
  | Sub Ty v v
  | Call Ty Str [v]
  | Const Ty Int64
  | Cmp Ty CmpOp v v
  | Val Ty v
  deriving (Show, Eq, Functor, Foldable, Traversable)

data InstrControl v
  = Jump (BlockCall v)
  | CondJump v (BlockCall v) (BlockCall v)
  | Ret v
  deriving (Show, Eq, Functor, Foldable, Traversable)

data BlockCall v = BlockCall
  { label :: Cfg.Label,
    args :: [v]
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Value = Value {ty :: Ty, name :: Cfg.Name}
  deriving (Show)

instance Eq Value where
  (==) = (==) `on` (.name)

instance Hashable Value where
  hashWithSalt salt = hashWithSalt salt . (.name)

instance Ord Value where
  compare = compare `on` (.name)

getOpInstrTy :: InstrOp v -> Ty
getOpInstrTy = \case
  Add ty _ _ -> ty
  Sub ty _ _ -> ty
  Call ty _ _ -> ty
  Cmp ty _ _ _ -> ty
  Const ty _ -> ty
  Val ty _ -> ty

compareValue :: Value -> Value -> Ordering
compareValue = compare `on` (\o -> Cfg.nameStr o.name)

-- mkVar :: Text -> Operand
-- mkVar = Var . Cfg.nameFromText

-- instance IsString Operand where
--   fromString = Var . fromString

data SomeInstr v where
  SomeInstr :: (Cfg.HasSControl c) => Instr v c -> SomeInstr v

deriving instance (Show op) => Show (SomeInstr op)

data Instr :: Type -> Control -> Type where
  BlockArgs :: [Value] -> Instr v E
  Assign :: v -> InstrOp v -> Instr v O
  Control :: InstrControl v -> Instr v C

deriving instance (Show op) => Show (Instr op c)

deriving instance (Eq op) => Eq (Instr op c)

pattern (:=) :: op -> (InstrOp op) -> Instr op O
pattern name := instr = Assign name instr

{-# COMPLETE (:=) #-}

makeFieldLabelsNoPrefix ''Function
makeFieldLabelsNoPrefix ''Value
makeFieldLabelsNoPrefix ''BlockCall

_BlockArgs :: Traversal' (Instr v E) [Value]
_BlockArgs = traversalVL $ \f -> \case
  BlockArgs names -> BlockArgs <$> f names

blockCalls :: Traversal' (Instr v C) (BlockCall v)
blockCalls = traversalVL $ \f (Control instr) ->
  Control <$> case instr of
    Jump bc -> Jump <$> f bc
    CondJump op bc1 bc2 -> CondJump op <$> f bc1 <*> f bc2
    Ret x -> pure $ Ret x

opInstrUses :: Traversal (InstrOp v) (InstrOp v') v v'
opInstrUses = traversalVL traverse

controlInstrUses :: Traversal (InstrControl v) (InstrControl v') v v'
controlInstrUses = traversalVL traverse

instrUses :: Traversal' (Instr v c) v
instrUses = traversalVL $ \f -> \case
  BlockArgs names -> pure $ BlockArgs names
  Assign name instr -> Assign <$> pure name <*> traverse f instr
  Control instr -> Control <$> traverse f instr

instrDefs :: Traversal' (Instr Value c) Value
instrDefs = traversalVL $ \f -> \case
  BlockArgs names -> BlockArgs <$> traverse f names
  Assign val instr -> Assign <$> f val <*> pure instr
  other -> pure other

instance Cfg.HasDefs (Instr Value op) Value where
  defs = (^.. instrDefs)

instance Cfg.HasUses (Instr Value c) Value where
  uses = (^.. instrUses)

instance Cfg.HasJumps (Instr v C) where
  jumps = (^.. blockCalls % to (.label))

runLiveness :: Graph -> Cfg.LabelMap (Set Value)
runLiveness = Cfg.runTransfer Cfg.livenessTransfer
