{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Backend.C
  ( genProgram,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cp.Check
import Cp.Syntax
import Data.Foldable (for_)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Optics
import Optics.State.Operators

-- import Prettyprinter hiding (Doc)
-- import Prettyprinter qualified as P

-- type Doc = P.Doc Void

data GenState = GenState
  { out :: [Text],
    scope :: HashMap LocalName Text
  }

data GenEnv = GenEnv
  { info :: ProgramInfo,
    indent :: Int,
    whitespace :: Bool
  }

makeFieldLabelsNoPrefix ''GenState
makeFieldLabelsNoPrefix ''GenEnv

type M = ReaderT GenEnv (State GenState)

mkGenState :: GenState
mkGenState = GenState {out = mempty, scope = mempty}

mkGenEnv :: ProgramInfo -> Bool -> GenEnv
mkGenEnv info whitespace = GenEnv {info, whitespace, indent = 0}

emit :: Text -> M ()
emit t = do
  #out %= (t :)

emitLn :: Text -> M ()
emitLn t = do
  emit t
  emit $ T.singleton '\n'

genProgram :: ProgramInfo -> Bool -> Program Typed -> Text
genProgram info whitespace program = T.concat $ reverse st.out
  where
    ((), st) = runState (runReaderT (genProgram' program) (mkGenEnv info whitespace)) mkGenState

-- freshName :: M Text
-- freshName = freshNameFrom "name"

genProgram' :: Program Typed -> M ()
genProgram' Program {decls} = do
  genIncludes
  for_ decls \decl -> do
    #scope .= mempty
    genDecl decl
    emitLn ""

toCType :: Type -> Text
toCType ty = case ty of
  PrimInt (IntType size signed) -> do
    let prefix = case signed of
          Unsigned -> "u"
          Signed -> ""
    prefix <> "int" <> (show (sizeToInt size)).t <> "_t"
  Void -> "void"
  _ -> undefined

genIncludes :: M ()
genIncludes = do
  emitLn "#include <stdint.h>"
  emitLn "#include <stdbool.h>"

delim :: Text -> Text -> M a -> M a
delim d d' m = do
  emit d
  x <- m
  emit d'
  pure x

parens :: M a -> M a
parens = delim "(" ")"

braces :: M a -> M a
braces = delim "{" "}"

genBuiltin :: BuiltinTyped -> M ()
genBuiltin builtin = case builtin of
  Add e1 e2 -> parens do
    genExpr e1
    emit "+"
    genExpr e2
  Mul e1 e2 -> parens do
    genExpr e1
    emit "*"
    genExpr e2
  Sub e1 e2 -> parens do
    genExpr e1
    emit "-"
    genExpr e2
  _ -> undefined

genLiteral :: Literal Typed -> M ()
genLiteral lit = case lit of
  Num num ty -> do
    parens do
      parens do
        emit $ toCType ty
      case num of
        Decimal {num} -> do
          emit $ (show num).t
        _ -> undefined
  _ -> undefined

genExpr :: Expr Typed -> M ()
genExpr expr = case expr of
  Builtin builtin -> genBuiltin builtin
  Call name args -> do
    case name of
      CallLocal local -> undefined
      CallTop name -> do
        emit name
    parens do
      commaList args \arg -> do
        genExpr arg
  Literal lit -> genLiteral lit
  Var var -> emit $ localNameToText var
  _ -> undefined

genStmt :: Stmt Typed -> M ()
genStmt stmt = case stmt of
  Return e -> do
    emit "return"
    case e of
      Nothing -> pure ()
      Just e -> genExpr e
    emit ";"
  ExprStmt expr -> do
    genExpr expr
    emit ";"
  Let name ty expr -> do
    emit $ toCType ty
    emit " "
    emit $ localNameToText name
    emit "="
    genExpr expr
    emit ";"
  _ -> undefined

commaList :: [a] -> (a -> M b) -> M ()
commaList ts f = forM_ (zip [0 ..] ts) \(i, t) -> do
  void $ f t
  when (i < len - 1) do
    emit ","
  where
    len = length ts

nameToText :: LocalName -> Text
nameToText (LocalName name id) = name <> (show id).t

genDecl :: Decl Typed -> M ()
genDecl decl = case decl of
  Fn name FnInfo {fnType = FnType {params = paramTypes, returnType}, params, body} -> do
    -- emit "static inline "
    emit $ toCType returnType
    emit " "
    emit name
    parens do
      commaList (zip params (fmap snd paramTypes)) \(name, ty) -> do
        emit $ toCType ty
        emit " "
        emit $ nameToText name
    case body of
      Just body ->
        braces do
          for_ body \stmt -> genStmt stmt
      Nothing -> emit ";"
  _ -> undefined
