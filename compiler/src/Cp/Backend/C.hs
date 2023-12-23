{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

#include "effectful.h"

module Cp.Backend.C
  ( genProgram,
  )
where

import Cp.Check
import Cp.Syntax
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Imports

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

state (GenState)
reader (GenEnv)

mkGenState :: GenState
mkGenState = GenState {out = mempty, scope = mempty}

mkGenEnv :: ProgramInfo -> Bool -> GenEnv
mkGenEnv info whitespace = GenEnv {info, whitespace, indent = 0}

emit :: (State' :> es) => Text -> Eff es ()
emit t = #out %= (t :)

emitLn :: (State' :> es) => Text -> Eff es ()
emitLn t = do
  emit t
  emit $ T.singleton '\n'

genProgram :: Bool -> ProgramTyped -> Text
genProgram whitespace program = T.concat $ reverse st.out
  where
    ((), st) =
      runPureEff $
        runState
          mkGenState
          ( runReader
              (mkGenEnv program.info whitespace)
              (genDecls program.decls)
          )

genDecls :: (State' :> es) => [Decl Typed] -> Eff es ()
genDecls decls = do
  genIncludes
  for_ decls \decl -> do
    #scope .= mempty
    genDecl decl
    emitLn ""

toCType :: Type -> Text
toCType = TB.runBuilder . go
  where
    go ty =
      case ty of
        NamedType name -> "struct " <> name.t.tb
        PrimInt (IntType size signed) -> do
          let prefix = case signed of
                Unsigned -> "u"
                Signed -> ""
          prefix <> "int" <> (show (sizeToInt size)).tb <> "_t"
        Void -> "void"
        PrimBool -> "bool"
        Pointer ty -> go ty <> "*"
        Array ArrayType {size, ty} -> "struct{" <> go ty <> " a[" <> (show size).tb <> "];}"
        Atomic ty -> "_Atomic(" <> go ty <> ")"
        Unknown -> error "should not get unknown"

emitType :: (State' :> es) => Type -> Eff es ()
emitType = emit <$> toCType

genIncludes :: (State' :> es) => Eff es ()
genIncludes = do
  emitLn "#include <stdint.h>"
  emitLn "#include <stdbool.h>"
  emitLn "#include <stdatomic.h>"

delim :: (State' :> es) => Text -> Text -> Eff es a -> Eff es a
delim d d' m = do
  emit d
  x <- m
  emit d'
  pure x

parens :: (State' :> es) => Eff es a -> Eff es a
parens = delim "(" ")"

braces :: (State' :> es) => Eff es a -> Eff es a
braces = delim "{" "}"

genBuiltin :: (State' :> es) => BuiltinTyped -> Eff es ()
genBuiltin (BuiltinIntCast toTy _fromTy expr) = do
  parens do
    parens do
      emitType (PrimInt toTy)
    genExpr expr
genBuiltin (BuiltinCast toTy _fromTy expr) = do
  parens do
    emitType toTy
    genExpr expr
genBuiltin (SomeBuiltin tag args) = case tag of
  Add -> bin "+"
  Mul -> bin "*"
  Sub -> bin "-"
  Eq -> bin "=="
  Lt -> bin "<"
  Le -> bin "<="
  Gt -> bin ">"
  Ge -> bin ">="
  _ -> todo
  where
    bin op = case args of
      [_, ExprArg e1, ExprArg e2] -> do
        genExpr e1
        emit op
        genExpr e2
      _ -> error "wrong args"

genLiteral :: (State' :> es) => Literal Typed -> Eff es ()
genLiteral lit = case lit of
  Num num ty -> do
    parens do
      parens do
        emitType (PrimInt ty)
      case num of
        Decimal {num} -> do
          emit (show num).t
        Hex {text} -> do
          emit "0x"
          emit text
  Bool b -> emit $ if b then "true" else "false"
  String s -> parens do
    emit "(uint8_t *)"
    emit "\""
    emit s
    emit "\""
  Char c -> todo

genExpr :: (State' :> es) => Expr Typed -> Eff es ()
genExpr expr = case expr of
  Builtin builtin -> genBuiltin builtin
  Ref expr -> do
    parens do
      emit "&"
      genExpr expr
  Deref expr -> do
    parens do
      emit "*"
      genExpr expr
  GetField expr name -> do
    parens do
      genExpr expr
      emit "."
      emit name.t
  Call name args -> do
    case name of
      CallLocal local -> todo
      CallTop name -> do
        emit name.t
    parens do
      commaList args \arg -> do
        genExpr arg
  Literal lit -> genLiteral lit
  Var var -> emit $ localNameToText var
  expr -> todoMsg $ "genExpr: " ++ show expr

genBody :: (State' :> es) => [Stmt Typed] -> Eff es ()
genBody body = braces $ mapM_ genStmt body

genIfCont :: (State' :> es) => IfCont Typed -> Eff es ()
genIfCont cont = case cont of
  ElseIf {cond, body, cont} -> do
    emit "else if"
    parens do genExpr cond
    genBody body
    genIfCont cont
  Else {body} -> do
    emit "else"
    genBody body
  NoIfCont -> pure ()

genStmt :: (State' :> es) => Stmt Typed -> Eff es ()
genStmt stmt = case stmt of
  Return e -> do
    emit "return"
    case e of
      Nothing -> pure ()
      Just e -> genExpr e
    emit ";"
  Loop body -> do
    emit "while (1)"
    genBody body
  If cond body cont -> do
    emit "if"
    parens do
      genExpr cond
    genBody body
    genIfCont cont
  ExprStmt expr -> do
    genExpr expr
    emit ";"
  Let name _ expr ty -> do
    emitType ty
    emit " "
    emit $ localNameToText name
    emit "="
    genExpr expr
    emit ";"
  _ -> todo

commaList :: (State' :> es) => [a] -> (a -> Eff es b) -> Eff es ()
commaList ts f = for_ (zip [0 ..] ts) \(i, t) -> do
  void $ f t
  when (i < len - 1) do
    emit ","
  where
    len = length ts

nameToText :: LocalName -> Text
nameToText = localNameToText

genDecl :: (State' :> es) => Decl Typed -> Eff es ()
genDecl decl = case decl of
  Fn name FnInfo {fnType = FnType {params = paramTypes, returnType}, params, body} -> do
    -- emit "static inline "
    emitType returnType
    emit " "
    emit name.t
    parens do
      commaList (zip params (fmap snd paramTypes)) \(name, ty) -> do
        emitType ty
        emit " "
        emit $ nameToText name
    case body of
      Just body ->
        braces do
          for_ body \stmt -> genStmt stmt
      Nothing -> emit ";"
  Struct name info -> do
    emit $ "struct " <> name.t
    braces do
      for_ info.fields \(name, ty) -> do
        emitType ty
        emit " "
        emit name.t
        emit ";"
    emit ";"
  _ -> todo
