module Backend.X86 where

import Cfg qualified
import Data.Kind qualified as Kind
import Imports

data X86 (reg :: Kind.Type)

instance Cfg.IsInstr (X86 reg) where
  type Instr (X86 reg) = Instr reg
  type InstrControl (X86 reg) = InstrControl reg
  type InstrBlockArgs (X86 reg) = [Cfg.Name]
  type InstrTemp (X86 reg) = VReg

data Instr reg
  = Lea (Operand reg) (Address reg)
  | Add (Operand reg) (Operand reg) (Operand reg)
  | Sub (Operand reg) (Operand reg) (Operand reg)
  | ParallelMove [(Operand reg, Operand reg)]
  | Mov (Operand reg) (Operand reg)

instrOperands :: Traversal' (Instr reg) (Operand reg)
instrOperands = traversalVL $ \f -> \case
  Lea op addr -> Lea <$> f op <*> pure addr
  Add op1 op2 op3 -> Add <$> f op1 <*> f op2 <*> f op3
  Sub op1 op2 op3 -> Sub <$> f op1 <*> f op2 <*> f op3
  ParallelMove ops -> ParallelMove <$> traverse (traverse f) ops
  Mov op1 op2 -> Mov <$> f op1 <*> f op2

data InstrControl reg
  = Jump Cfg.Label
  | CondJump Cfg.Label Cfg.Label
  | Ret
  deriving (Show, Eq)

instrControlLabels :: Traversal' (InstrControl reg) Cfg.Label
instrControlLabels = traversalVL $ \f -> \case
  Jump label -> Jump <$> f label
  CondJump label1 label2 -> CondJump <$> f label1 <*> f label2
  Ret -> pure Ret

type Block reg = Cfg.Block (X86 reg)

type Graph reg = Cfg.Graph (X86 reg)

instance Cfg.HasJumps (InstrControl reg) where
  jumps instr = instr ^.. instrControlLabels

instance Cfg.HasDefs (Instr reg) reg where
  defs instr = instr ^.. instrOperands % _Reg

data Operand reg
  = Imm Int32
  | Reg reg
  | Mem (Address reg)

_Reg :: Prism' (Operand reg) reg
_Reg = prism' Reg $ \case
  Reg reg -> Just reg
  _ -> Nothing

type VReg = Cfg.Name

-- https://blog.yossarian.net/2020/06/13/How-x86_64-addresses-memory
-- base + (index * scale) + displacement
data Address reg = Address
  { base :: reg,
    index :: reg,
    scale :: Scale,
    displacement :: Int32
  }

data Scale = Scale1 | Scale2 | Scale4 | Scale8
