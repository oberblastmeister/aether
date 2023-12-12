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
import Imports
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB

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

genProgram :: Bool -> ProgramTyped -> Text
genProgram whitespace program = T.concat $ reverse st.out
  where
    ((), st) =
      runState
        ( runReaderT
            (genDecls program.decls)
            (mkGenEnv program.info whitespace)
        )
        mkGenState

genDecls :: [Decl Typed] -> M ()
genDecls decls = do
  genIncludes
  for_ decls \decl -> do
    #scope .= mempty
    genDecl decl
    emitLn ""

toCType :: Type -> M Text
toCType ty = do
  env <- ask
  let go ty =
        case ty of
          NamedType name -> do
            let struct = env.info.structs ^?! ix name
            "struct {"
              <> foldr (\(field, ty) b -> go ty <> " " <> field.tb <> ";" <> b) "" struct.fields
              <> "}"
          PrimInt (IntType size signed) -> do
            let prefix = case signed of
                  Unsigned -> "u"
                  Signed -> ""
            prefix <> "int" <> (show (sizeToInt size)).tb <> "_t"
          Void -> "void"
          PrimBool -> "bool"
          Pointer ty -> go ty <> "*"
          Array ArrayType {size, ty} -> "struct{" <> go ty <> " a[" <> (show size).tb <> "];}"
          Unknown -> error "should not get unknown"
  pure . TB.runBuilder . go $ ty
  where

emitType :: Type -> M ()
emitType = emit <=< toCType

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
genBuiltin (SomeBuiltin tag args) = case tag of
  Add -> bin "+"
  Mul -> bin "*"
  Sub -> bin "-"
  Eq -> bin "=="
  Lt -> bin "<"
  Le -> bin "<="
  Gt -> bin ">"
  Ge -> bin ">="
  _ -> undefined
  where
    bin op = case args of
      [_, Right e1, Right e2] -> do
        genExpr e1
        emit op
        genExpr e2
      _ -> error "wrong args"

genLiteral :: Literal Typed -> M ()
genLiteral lit = case lit of
  Num num ty -> do
    parens do
      parens do
        emitType ty
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

genBody :: [Stmt Typed] -> M ()
genBody body = braces $ mapM_ genStmt body

genIfCont :: IfCont Typed -> M ()
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

genStmt :: Stmt Typed -> M ()
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
  Let name ty expr -> do
    emitType ty
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
nameToText  = localNameToText

genDecl :: Decl Typed -> M ()
genDecl decl = case decl of
  Fn name FnInfo {fnType = FnType {params = paramTypes, returnType}, params, body} -> do
    -- emit "static inline "
    emitType returnType
    emit " "
    emit name
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
  _ -> undefined
