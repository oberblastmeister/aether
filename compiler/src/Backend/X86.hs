module Backend.X86 where

import Backend.X86.Register
import Cfg (Control (..))
import Cfg qualified
import Data.Kind (Type)
import Imports

data OpInstr reg
  = Lea reg (Address reg)
  | Add (Operand reg) (Operand reg) (Operand reg)
  | Sub (Operand reg) (Operand reg) (Operand reg)
  | ParallelMove [(Operand reg, Operand reg)]
  | Mov (Operand reg) (Operand reg)

instrRegs :: Traversal (OpInstr reg) (OpInstr reg') reg reg'
instrRegs = traversalVL $ \f -> \case
  Lea reg addr -> Lea <$> f reg <*> traverse f addr
  Add op1 op2 op3 -> Add <$> traverse f op1 <*> traverse f op2 <*> traverse f op3
  Sub op1 op2 op3 -> Sub <$> traverse f op1 <*> traverse f op2 <*> traverse f op3
  ParallelMove ops -> ParallelMove <$> traverse (\(x, y) -> (,) <$> traverse f x <*> traverse f y) ops
  Mov op1 op2 -> Mov <$> traverse f op1 <*> traverse f op2

data BlockCall reg = BlockCall
  { label :: Cfg.Label,
    args :: [Operand reg]
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

data JumpKind

data InstrControl reg
  = Jump Cfg.Label
  | CondJump Cfg.Label Cfg.Label
  | Ret
  deriving (Show, Eq)

data Instr :: Type -> Control -> Type where
  BlockArgs :: [reg] -> Instr reg E
  Instr :: OpInstr reg -> OpInstr reg -> Instr reg O
  Control :: InstrControl reg -> Instr reg C

instrControlLabels :: Traversal' (InstrControl reg) Cfg.Label
instrControlLabels = traversalVL $ \f -> \case
  Jump label -> Jump <$> f label
  CondJump label1 label2 -> CondJump <$> f label1 <*> f label2
  Ret -> pure Ret

-- type Block reg = Cfg.Block (X86 reg)

-- type Graph reg = Cfg.Graph (X86 reg)

-- instance Cfg.HasJumps (InstrControl reg) where
--   jumps instr = instr ^.. instrControlLabels

-- instance Cfg.HasDefs (Instr reg) reg where
--   defs instr = instr ^.. instrOperands % _Reg

data Operand reg
  = Imm Int32
  | Reg reg
  | Mem (Address reg)
  deriving (Show, Eq, Functor, Foldable, Traversable)

operandRegs :: Traversal (Operand reg) (Operand reg') reg reg'
operandRegs = traversalVL traverse

_Reg :: Prism' (Operand reg) reg
_Reg = prism' Reg $ \case
  Reg reg -> Just reg
  _ -> Nothing

data StackOffset = StackOffEnd Int32 | StackOffBase Int32
  deriving (Show, Eq)

data Location
  = InReg Register
  | InStack (Maybe StackOffset)

data VReg
  = VReg Cfg.Name
  | PreColored Cfg.Name Register
  | PreStack
      -- offset from end
      (Maybe Int32)

-- https://blog.yossarian.net/2020/06/13/How-x86_64-addresses-memory
-- base + (index * scale) + displacement
data Address reg = Address
  { base :: reg,
    index :: reg,
    scale :: Scale,
    displacement :: Int32
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

addressRegs :: Traversal (Address reg) (Address reg') reg reg'
addressRegs = traversalVL traverse

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Show, Eq)
