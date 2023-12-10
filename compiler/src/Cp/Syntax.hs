{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Syntax where

import Data.HashMap.Internal.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Kind (Constraint)
import Data.Kind qualified as Kind
import Data.Text (Text)
import Dot ()
import GHC.Generics (Generic)
import Prelude hiding (Num)
import Optics

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

type PhaseC (c :: Kind.Type -> Constraint) p =
  ( c (TypeAnn p),
    c (BuiltinPhase p),
    c (Var p),
    c (CallVar p)
  )

data Literal p
  = String Text
  | Char Text
  | Num
      (NumLiteral)
      (TypeAnn p)
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

data BuiltinParsed = BuiltinParsed
  { name :: Text,
    args :: [Either Type (Expr Parsed)]
  }
  deriving (Show, Eq)

data CompareType = CompareInt IntType | CompareBool
  deriving (Show, Eq)

data BuiltinTyped
  = Eq (Expr Typed) (Expr Typed)
  | Lt (Expr Typed) (Expr Typed)
  | Le (Expr Typed) (Expr Typed)
  | Gt (Expr Typed) (Expr Typed)
  | Ge (Expr Typed) (Expr Typed)
  | Add (Expr Typed) (Expr Typed)
  | Mul (Expr Typed) (Expr Typed)
  | Sub (Expr Typed) (Expr Typed)
  | AddOverflow (Expr Typed) (Expr Typed)
  | MulOverflow (Expr Typed) (Expr Typed)
  | SubOverflow (Expr Typed) (Expr Typed)
  | And (Expr Typed) (Expr Typed)
  | Or (Expr Typed) (Expr Typed)
  | Not (Expr Typed)
  | Shl Bool (Expr Typed) (Expr Typed)
  | Shr Bool (Expr Typed) (Expr Typed)
  | BitNot (Expr Typed)
  | BitOr (Expr Typed) (Expr Typed)
  | BitAnd (Expr Typed) (Expr Typed)
  | Cast Type (Expr Typed)
  | As Type (Expr Typed)
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

data Expr p
  = Call (CallVar p) [Expr p]
  | Builtin (BuiltinPhase p)
  | SpannedExpr (Expr p) SourcePos
  | Null
  | PlaceExpr (Place p)
  | Ref (Expr p)
  | Literal (Literal p)
  | StructLiteral [(Maybe Text, (Expr p))] (TypeAnn p)
  | Error
  | EnumLiteral Text (TypeAnn p)
  | Var (Var p)

deriving instance (PhaseC Show p) => Show (Expr p)

deriving instance (PhaseC Eq p) => Eq (Expr p)

data Size = U8 | U16 | U32 | U64
  deriving (Show, Eq)

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
  | Void
  | Unknown
  | Array ArrayType
  deriving (Show, Eq)

data IfCont p
  = ElseIf
      { cond :: (Expr p),
        body :: [Stmt p]
      }
  | Else {body :: [Stmt p]}

deriving instance (PhaseC Show p) => Show (IfCont p)

deriving instance (PhaseC Eq p) => Eq (IfCont p)

data Stmt p
  = If (Expr p) [Stmt p] (IfCont p)
  | Loop [Stmt p]
  | ExprStmt (Expr p)
  | Let (Var p) Type (Expr p)
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

-- deriving instance (PhaseC Show p) => Show (FnType )

-- deriving instance (PhaseC Eq p) => Eq (FnType )

data FnInfo p = FnInfo
  { fnType :: FnType,
    params :: [Var p],
    body :: Maybe [Stmt p]
  }

deriving instance (PhaseC Show p) => Show (FnInfo p)

deriving instance (PhaseC Eq p) => Eq (FnInfo p)

data Decl p
  = Struct Text (StructInfo)
  | Fn Text (FnInfo p)
  | Enum Text (EnumInfo)
  | Union Text (UnionInfo)
  | SpannedDecl (Decl p) SourcePos

deriving instance (PhaseC Show p) => Show (Decl p)

deriving instance (PhaseC Eq p) => Eq (Decl p)

data Program p = Program
  { decls :: [Decl p]
  }

deriving instance (PhaseC Show p) => Show (Program p)

deriving instance (PhaseC Eq p) => Eq (Program p)

makeFieldLabelsNoPrefix ''StructInfo
