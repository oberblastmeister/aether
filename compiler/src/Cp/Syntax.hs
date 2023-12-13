{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Syntax where

import Data.Kind qualified as Kind
import Imports
import Prelude hiding (Num)

data SourcePos = SourcePos Int
  deriving (Show, Eq)

data NumLiteral
  = Decimal
      { num :: Integer
      }
  | Hex
      { text :: Text,
        num :: Integer
      }
  deriving (Show, Eq)

type family TypeAnn p where
  TypeAnn Parsed = ()
  TypeAnn Typed = Type

type family IntTypeAnn p where
  IntTypeAnn Parsed = ()
  IntTypeAnn Typed = IntType

type PhaseC (c :: Kind.Type -> Constraint) p =
  ( c (TypeAnn p),
    c (IntTypeAnn p),
    c (BuiltinPhase p),
    c (Var p),
    c (CallVar p),
    c (OnlyOnPhase Parsed p),
    c (OnlyOnPhase Typed p)
  )

data Literal p
  = String Text
  | Char Text
  | Num
      (NumLiteral)
      (IntTypeAnn p)
  | Bool Bool

deriving instance (PhaseC Show p) => Show (Literal p)

deriving instance (PhaseC Eq p) => Eq (Literal p)

-- builtinFromText :: Text -> Maybe Builtin
-- builtinFromText = \case
--   "not" -> Just BuiltinNot
--   _ -> Nothing

data PlaceContext
  = GetField PlaceContext Text
  | Deref PlaceContext
  | PlaceHole
  deriving (Show, Eq)

type data Phase = Parsed | Typed

type BuiltinArgs p = [Either Type (Expr p)]

data BuiltinParsed = BuiltinParsed
  { name :: Text,
    args :: BuiltinArgs Parsed
  }
  deriving (Show, Eq)

data CompareType = CompareInt IntType | CompareBool
  deriving (Show, Eq)

data BinBuiltin p = BinBuiltin Type (Expr p) (Expr p)

deriving instance (PhaseC Show p) => Show (BinBuiltin p)

deriving instance (PhaseC Eq p) => Eq (BinBuiltin p)

data UnaryBuiltin p = UnaryBuiltin Type (Expr p)

deriving instance (PhaseC Show p) => Show (UnaryBuiltin p)

deriving instance (PhaseC Eq p) => Eq (UnaryBuiltin p)

data BuiltinTag
  = Eq
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Mul
  | Sub
  | AddOverflow
  | MulOverflow
  | SubOverflow
  | And
  | Or
  | Not
  | Shl
  | Shr
  | BitNot
  | BitOr
  | BitAnd
  deriving (Show, Eq, Enum, Bounded)

data BuiltinTyped = SomeBuiltin BuiltinTag (BuiltinArgs Typed)
  deriving (Show, Eq)

type family BuiltinPhase p where
  BuiltinPhase Parsed = BuiltinParsed
  BuiltinPhase Typed = BuiltinTyped

data LocalName = LocalName Text Int
  deriving (Show, Eq, Generic)

instance Hashable LocalName

localNameToText :: LocalName -> Text
localNameToText (LocalName name id) = name <> (show id).t

data VarResolved = VarLocal LocalName
  deriving (Show, Eq)

type family Var p where
  Var Parsed = Text
  Var Typed = LocalName

data CallVarTyped = CallTop Text | CallLocal LocalName
  deriving (Show, Eq)

type family CallVar p where
  CallVar Parsed = Text
  CallVar Typed = CallVarTyped

data Place p = Place (Expr p) PlaceContext

deriving instance (PhaseC Show p) => Show (Place p)

deriving instance (PhaseC Eq p) => Eq (Place p)

type family OnlyOnPhase p q where
  OnlyOnPhase Parsed Parsed = ()
  OnlyOnPhase Typed Typed = ()
  OnlyOnPhase _ _ = Void

data Expr p
  = Call (CallVar p) [Expr p]
  | Builtin (BuiltinPhase p)
  | SpannedExpr (Expr p) SourcePos
  | As Type (Expr p)
  | Cast Type Type (Expr p)
  | IntCast IntType IntType (Expr p) (OnlyOnPhase Typed p)
  | Null
  | PlaceExpr (Place p)
  | Ref (Expr p)
  | Literal (Literal p)
  | BraceLiteral (Fields p) (OnlyOnPhase Parsed p)
  | ArrayLiteral [Expr p] (OnlyOnPhase Typed p)
  | StructLiteral
      [(Text, Expr Typed)]
      (HashMap Text (Expr Typed))
      StructInfo
      Text
      (OnlyOnPhase Typed p)
  | VoidLiteral (OnlyOnPhase Typed p)
  | Error
  | EnumLiteral Text (TypeAnn p)
  | Var (Var p)

type Fields p = [(Maybe Text, (Expr p))]

deriving instance (PhaseC Show p) => Show (Expr p)

deriving instance (PhaseC Eq p) => Eq (Expr p)

data Size = U8 | U16 | U32 | U64
  deriving (Show, Eq, Ord, Bounded, Enum)

sizeToInt :: Size -> Int
sizeToInt size = case size of
  U8 -> 8
  U16 -> 16
  U32 -> 32
  U64 -> 64

-- sizeToBounds :: Size -> (Integer, Integer)
-- sizeToBounds size = case size of

data Signedness = Signed | Unsigned
  deriving (Show, Eq)

data IntType = IntType Size Signedness
  deriving (Show, Eq)

data ArrayType = ArrayType
  { size :: Integer,
    ty :: Type
  }
  deriving (Show, Eq)

data Type
  = NamedType Text
  | PrimInt IntType
  | PrimBool
  | Pointer Type
  | Atomic Type
  | Void
  | Unknown
  | Array ArrayType
  deriving (Show, Eq)

data IfCont p
  = ElseIf
      { cond :: (Expr p),
        body :: [Stmt p],
        cont :: (IfCont p)
      }
  | Else {body :: [Stmt p]}
  | NoIfCont

deriving instance (PhaseC Show p) => Show (IfCont p)

deriving instance (PhaseC Eq p) => Eq (IfCont p)

-- data PhaseMaybe :: Phase -> Type -> Type where
--   PhaseJust :: forall p a. a -> PhaseMaybe p a
--   PhaseNothing :: forall p a. PhaseMaybe p a

data Stmt p
  = If (Expr p) [Stmt p] (IfCont p)
  | Loop [Stmt p]
  | ExprStmt (Expr p)
  | Let (Var p) (Maybe Type) (Expr p) (TypeAnn p)
  | Set (Place p) (Expr p)
  | Return (Maybe (Expr p))
  | Break

deriving instance (PhaseC Show p) => Show (Stmt p)

deriving instance (PhaseC Eq p) => Eq (Stmt p)

data StructInfo = StructInfo
  { fields :: [(Text, Type)],
    fieldMap :: HashMap Text Type
  }
  deriving (Show, Eq)

data UnionInfo = UnionInfo
  { unions :: [(Text, Type)]
  }
  deriving (Show, Eq)

data EnumInfo = EnumInfo
  { repType :: IntType,
    tags :: [(Text, Maybe NumLiteral)]
  }
  deriving (Show, Eq)

data FnType = FnType
  { params :: [(Text, Type)],
    returnType :: Type
  }
  deriving (Show, Eq)

data FnInfo p = FnInfo
  { fnType :: FnType,
    params :: [Var p],
    body :: Maybe [Stmt p]
  }

deriving instance (PhaseC Show p) => Show (FnInfo p)

deriving instance (PhaseC Eq p) => Eq (FnInfo p)

data Decl p
  = Struct Text StructInfo
  | Fn Text (FnInfo p)
  | Enum Text (EnumInfo)
  | Union Text (UnionInfo)
  | SpannedDecl (Decl p) SourcePos

deriving instance (PhaseC Show p) => Show (Decl p)

deriving instance (PhaseC Eq p) => Eq (Decl p)

data ProgramParsed = ProgramParsed
  { decls :: [Decl Parsed]
  }
  deriving (Show, Eq)

data ProgramInfo = ProgramInfo
  { structs :: HashMap Text (StructInfo),
    enums :: HashMap Text (EnumInfo),
    unions :: HashMap Text (UnionInfo),
    fns :: HashMap Text FnType
  }
  deriving (Show, Eq)

data ProgramTyped = ProgramTyped
  { info :: ProgramInfo,
    decls :: [Decl Typed]
  }
  deriving (Show, Eq)

makePrismLabels ''Type
makeFieldLabelsNoPrefix ''StructInfo
