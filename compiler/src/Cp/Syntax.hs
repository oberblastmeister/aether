{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Syntax where

import Data.Kind qualified as Kind
import Data.Str (Str)
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
    c (OnlyOnPhase Typed p),
    c (BraceLiteral p),
    c (ExprOrLValue p)
  )

data Literal p
  = String Text
  | -- TODO: make sure this is ascii
    Char Text
  | Num
      (NumLiteral)
      (IntTypeAnn p)
  | Bool Bool

deriving instance (PhaseC Show p) => Show (Literal p)

deriving instance (PhaseC Eq p) => Eq (Literal p)

data PlaceContext
  = CxGetField PlaceContext Text
  | CxDeref PlaceContext
  | CxHole
  deriving (Show, Eq)

type data Phase = Parsed | Typed

data SPhase p where
  SParsed :: SPhase Parsed
  STyped :: SPhase Typed

class SingPhase (p :: Phase) where
  singPhase :: SPhase p

data BuiltinParsed = BuiltinParsed
  { name :: Str,
    args :: [BuiltinArg Parsed]
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
  | AtomicFetchAdd
  deriving (Show, Eq, Enum, Bounded)

data BuiltinArg p
  = TypeArg Type
  | ExprArg (Expr p)
  | IdentArg Str

deriving instance (PhaseC Show p) => Show (BuiltinArg p)

deriving instance (PhaseC Eq p) => Eq (BuiltinArg p)

data BuiltinTyped
  = SomeBuiltin BuiltinTag [BuiltinArg Typed]
  | BuiltinCast Type Type (Expr Typed)
  | BuiltinIntCast IntType IntType (Expr Typed)
  deriving (Show, Eq)

type family BuiltinPhase p where
  BuiltinPhase Parsed = BuiltinParsed
  BuiltinPhase Typed = BuiltinTyped

data LocalName = LocalName Str Int
  deriving (Show, Eq, Generic)

instance Hashable LocalName

localNameToText :: LocalName -> Text
localNameToText (LocalName name id) = name.t <> (show id).t

data VarResolved = VarLocal LocalName
  deriving (Show, Eq)

type family Var p where
  Var Parsed = Str
  Var Typed = LocalName

data CallVarTyped = CallTop Str | CallLocal LocalName
  deriving (Show, Eq)

type family CallVar p where
  CallVar Parsed = Str
  CallVar Typed = CallVarTyped

data LValue
  = LValueDeref (Expr Typed)
  | LValueGetField LValue Str
  | LValueVar (Var Typed)
  deriving (Show, Eq)

type family ExprOrLValue p where
  ExprOrLValue Parsed = Expr Parsed
  ExprOrLValue Typed = LValue

type family OnlyOnPhase p q where
  OnlyOnPhase Parsed Parsed = ()
  OnlyOnPhase Typed Typed = ()
  OnlyOnPhase _ _ = Void

type family BraceLiteral p where
  BraceLiteral Parsed = Fields Parsed
  BraceLiteral Typed = BraceTyped

data BraceTyped
  = ArrayLiteral [Expr Typed]
  | StructLiteral
      [(Str, Expr Typed)]
      (HashMap Str (Expr Typed))
      StructInfo
      Str
  | VoidLiteral
  deriving (Show, Eq)

data Expr p
  = Call (CallVar p) [Expr p]
  | Builtin (BuiltinPhase p)
  | SpannedExpr (Expr p) SourcePos
  | Deref (Expr p)
  | GetField (Expr p) Str
  | As Type (Expr p)
  | NullPtr
  | Ref (Expr p)
  | Literal (Literal p)
  | BraceLiteral (BraceLiteral p)
  | Error
  | EnumLiteral Str (TypeAnn p)
  | Var (Var p)

type Fields p = [(Maybe Str, (Expr p))]

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
  = NamedType Str
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

data Stmt p
  = If (Expr p) [Stmt p] (IfCont p)
  | Loop [Stmt p]
  | ExprStmt (Expr p)
  | Let (Var p) (Maybe Type) (Expr p) (TypeAnn p)
  | Label Str
  | Goto Str
  | Set (ExprOrLValue p) (Expr p)
  | Return (Maybe (Expr p))
  | Break

deriving instance (PhaseC Show p) => Show (Stmt p)

deriving instance (PhaseC Eq p) => Eq (Stmt p)

data StructInfo = StructInfo
  { fields :: [(Str, Type)],
    fieldMap :: HashMap Str Type
  }
  deriving (Show, Eq)

data UnionInfo = UnionInfo
  { unions :: [(Str, Type)]
  }
  deriving (Show, Eq)

data EnumInfo = EnumInfo
  { repType :: IntType,
    tags :: [(Str, Maybe NumLiteral)]
  }
  deriving (Show, Eq)

data FnType = FnType
  { params :: [(Str, Type)],
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
  = Struct Str StructInfo
  | Fn Str (FnInfo p)
  | Enum Str (EnumInfo)
  | Union Str (UnionInfo)
  | SpannedDecl (Decl p) SourcePos

deriving instance (PhaseC Show p) => Show (Decl p)

deriving instance (PhaseC Eq p) => Eq (Decl p)

data ProgramParsed = ProgramParsed
  { decls :: [Decl Parsed]
  }
  deriving (Show, Eq)

data ProgramInfo = ProgramInfo
  { structs :: HashMap Str (StructInfo),
    enums :: HashMap Str (EnumInfo),
    unions :: HashMap Str (UnionInfo),
    fns :: HashMap Str FnType
  }
  deriving (Show, Eq)

data ProgramTyped = ProgramTyped
  { info :: ProgramInfo,
    decls :: [Decl Typed]
  }
  deriving (Show, Eq)

makePrismLabels ''Type
makeFieldLabelsNoPrefix ''StructInfo
makePrismLabels ''BuiltinArg
