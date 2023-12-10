{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Check
  ( ProgramInfo (..),
    checkProgram,
  )
where

import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cp.Syntax
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Traversable (for)
import Debug.Trace
import Debug.Trace (traceShowM)
import Dot ()
import Optics
import Optics.Operators.Unsafe ((^?!))
import Optics.State.Operators

data Scope = Scope
  { mapping :: HashMap Text (Type, LocalName)
  }

data CheckState = CheckState
  { structs :: HashMap Text (StructInfo),
    enums :: HashMap Text (EnumInfo),
    unions :: HashMap Text (UnionInfo),
    fns :: HashMap Text FnType,
    pos :: SourcePos,
    errors :: [CheckError],
    scopes :: [Scope],
    unique :: Int
  }

data CheckError = CheckError
  { error :: Text,
    pos :: SourcePos
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''Scope
makeFieldLabelsNoPrefix ''CheckState

mkCheckState :: CheckState
mkCheckState =
  CheckState
    { structs = mempty,
      enums = mempty,
      unions = mempty,
      fns = mempty,
      pos = SourcePos 0,
      errors = [],
      scopes = [],
      unique = 0
    }

data CheckEnv = CheckEnv
  { inLoop :: Bool,
    variables :: HashMap Text Type,
    returnType :: Maybe Type
  }

mkCheckEnv :: CheckEnv
mkCheckEnv =
  CheckEnv
    { inLoop = False,
      variables = mempty,
      returnType = Nothing
    }

type M = ReaderT CheckEnv (State CheckState)

newScope :: M a -> M a
newScope m = do
  #scopes %= (Scope {mapping = mempty} :)
  x <- m
  use #scopes >>= \case
    _ : scopes -> do
      #scopes .= scopes
      pure x
    [] -> error "should not have empty scope"

addScope :: Text -> (Type, LocalName) -> M ()
addScope name localName = #scopes % _head % #mapping % at name .= Just localName

lookupScope :: Text -> M (Maybe (Type, LocalName))
lookupScope name = do
  go <$> use #scopes
  where
    go (scope : scopes)
      | Just name' <- scope ^. #mapping % at name = Just name'
      | otherwise = go scopes
    go [] = Nothing

freshNameFrom :: Text -> M LocalName
freshNameFrom name = do
  u <- use #unique
  #unique %= (+ 1)
  pure $ LocalName name u

lookupType :: CheckState -> Text -> Bool
lookupType st n = (has (at n) st.structs) || (has (at n) st.enums) || (has (at n) st.unions)

checkType :: Type -> M ()
checkType ty = do
  st <- get
  case ty of
    NamedType name -> case lookupType st name of
      True -> pure ()
      False -> checkError $ "could not find type: ".t <> name
    Pointer ty -> checkType ty
    _ -> pure ()

checkError :: Text -> M ()
checkError error = do
  pos <- use #pos
  #errors %= (CheckError {error, pos} :)

findDuplicate :: forall a. [(Text, a)] -> Either Text (HashMap Text a)
findDuplicate = go (Empty :: HashMap Text a)
  where
    go m ((t, x) : ts) =
      if has (ix t) m
        then Left t
        else go (m & at t .~ Just x) ts
    go m [] = Right m

checkDuplicate :: [(Text, a)] -> (Text -> Text) -> M ()
checkDuplicate ts msg = case findDuplicate ts of
  Right _ -> pure ()
  Left t -> checkError (msg t)

-- actual, expected
unifyType :: Type -> Type -> M ()
unifyType ty1 ty2 = unless (ty1 == ty2) do
  checkError $ ("expected type " <> (show ty2) <> ", got " <> (show ty1)).t

u8Ty :: Type
u8Ty = PrimInt (IntType U8 Unsigned)

expectError :: Text -> Text -> M ()
expectError actual expected = checkError $ "expected ".t <> expected <> ", got ".t <> actual

checkLiteral :: Literal Parsed -> Type -> M (Literal Typed)
checkLiteral lit ty = do
  traceM "checking literal"
  case lit of
    Num lit () -> do
      case ty of
        PrimInt _ -> pure ()
        _ -> expectError "integer".t (show ty).t
      pure $ Num lit ty
    _ -> do
      (ty', lit) <- inferLiteral lit
      unifyType ty ty'
      pure lit

inferPlace :: Place Parsed -> M (Type, Place Typed)
inferPlace (Place expr place) = do
  (ty, expr) <- inferExpr expr
  ty <- go ty place
  pure $ (ty, Place expr place)
  where
    go ty place = case place of
      Deref place -> case ty of
        Pointer ty -> go ty place
        _ -> do
          expectError "pointer".t (show ty).t
          pure Unknown
      PlaceHole -> pure ty
      GetField place field ->
        case ty of
          NamedType name -> do
            st <- get
            let struct = st ^?! #structs % ix name
            case struct.fieldMap ^. at name of
              Nothing -> do
                checkError $ "could not field field ".t <> (show field).t <> " in struct ".t <> name
                pure Unknown
              Just ty -> go ty place
          _ -> do
            expectError "named".t (show ty).t
            pure Unknown

-- inferCall :: CallExpr -> M (Type, Expr)
-- inferCall CallExpr {name, args} ty = do
--   undefined

-- checkBuiltin :: Text -> [Either Type (Expr Parsed)] -> [Either Type (Expr Typed)]
-- checkBuiltin _ _ = undefined

-- inferExpr :: Expr Parsed -> M (Type, Expr Typed)
-- inferExpr expr = case expr of
--   Call call args -> undefined
--   StructLiteral fields ty -> pure (Unknown, expr)
--   Literal lit -> (,expr) <$> inferLiteral lit
--   Null -> pure (Unknown, expr)
--   Place expr place -> do
--     inferExpr expr

inferBuiltin :: BuiltinParsed -> M (Type, Expr Typed)
inferBuiltin BuiltinParsed {name, args} = case name.s of
  "add" -> bin checkIntType Add
  "sub" -> bin checkIntType Sub
  "mul" -> bin checkIntType Mul
  "eq" -> bin checkIntType Eq
  "lt" -> bin checkIntType Lt
  "le" -> bin checkIntType Le
  "ge" -> bin checkIntType Ge
  "gt" -> bin checkIntType Gt
  _ -> undefined
  where
    bin checkTy f = case args of
      [Left ty, Right e, Right e'] -> do
        checkTy ty
        e <- checkExpr e ty
        e' <- checkExpr e' ty
        pure (ty, Builtin $ f e e')
      _ -> do
        checkError $ "invalid arguments for ".t <> name
        pure (Unknown, Error)
    unary f = case args of
      [Left ty, Right e, Right e'] -> do
        checkIntType ty
        e <- checkExpr e ty
        e' <- checkExpr e' ty
        pure (ty, Builtin $ f e e')
      _ -> do
        checkError $ "invalid arguments for ".t <> name
        pure (Unknown, Error)

    checkIntType ty = case ty of
      PrimInt _ -> pure ()
      _ -> expectError (show ty).t "int".t

inferExpr :: Expr Parsed -> M (Type, Expr Typed)
inferExpr expr = case expr of
  Call call args -> do
    fn <- use (#fns % at call)
    case fn of
      Nothing -> do
        checkError $ "could not find function ".t <> call
        pure (Unknown, Error)
      Just FnType {params, returnType} -> do
        let argLen = length @[] args
        let paramLen = length params
        if argLen /= paramLen
          then do
            checkError $
              "expected ".t
                <> (show paramLen).t
                <> " arguments, got ".t
                <> (show argLen).t
            pure (Unknown, Error)
          else do
            args <- for (zip args (fmap snd params)) \(arg, param) -> checkExpr arg param
            pure (returnType, Call (CallTop call) args)
  Literal lit -> (fmap . fmap) Literal (inferLiteral lit)
  Var var ->
    lookupScope var >>= \case
      Nothing -> do
        checkError $ "var ".t <> var <> " not found".t
        pure (Unknown, Error)
      Just res -> pure $ Var <$> res
  Ref expr -> undefined
  PlaceExpr place -> (fmap . fmap) PlaceExpr (inferPlace place)
  Builtin builtin -> inferBuiltin builtin
  _ -> pure (Unknown, Error)

checkExpr :: Expr Parsed -> Type -> M (Expr Typed)
checkExpr expr ty = do
  traceM $ "checking expr: " ++ show expr ++ " ty: " ++ show ty
  case expr of
    Literal lit -> do
      Literal <$> checkLiteral lit ty
    PlaceExpr (Place expr place) -> do
      case place of
        PlaceHole -> checkExpr expr ty
        _ -> undefined
    StructLiteral fields () -> do
      case ty of
        NamedType name -> do
          info <- use (#structs % at name)
          -- case info of
          --   Nothing -> error "impossible"
          --   Just info -> undefined
          pure Error

        -- case lookupType st name of
        --   Nothing ->
        --   Just

        _ -> do
          checkError "expected namedtype".t
          pure Error
    _ -> do
      (ty', expr) <- inferExpr expr
      unifyType ty' ty
      pure expr

unknownTypeError :: M ()
unknownTypeError = checkError "cannot infer type".t

inferLiteral :: Literal Parsed -> M (Type, Literal Typed)
inferLiteral lit =
  case lit of
    String s -> pure $ (Pointer u8Ty, String s)
    Char c -> pure $ (u8Ty, Char c)
    Num num () -> do
      unknownTypeError
      pure (Unknown, Num num Unknown)
    Bool b -> pure (PrimBool, Bool b)

checkTypeNotDuplicate :: Text -> M ()
checkTypeNotDuplicate name = do
  st <- get
  case lookupType st name of
    True -> checkError $ ("duplicate type: ".t <> name)
    False -> pure ()

checkCont :: IfCont Parsed -> Type -> M (IfCont Typed)
checkCont ic ty = case ic of
  ElseIf {cond, body} -> do
    cond <- checkExpr cond PrimBool
    body <- checkBody body ty
    pure ElseIf {cond, body}
  Else {body} -> do
    body <- checkBody body ty
    pure Else {body}

-- the type is the type of the return
checkStmt :: Stmt Parsed -> Type -> M (Stmt Typed)
checkStmt stmt returnTy = case stmt of
  If cond body cont -> do
    cond <- checkExpr cond PrimBool
    body <- checkBody body returnTy
    cont <- checkCont cont returnTy
    pure $ If cond body cont
  Loop body -> do
    body <- checkBody body returnTy
    pure $ Loop body
  ExprStmt e -> ExprStmt <$> checkExpr e Void
  Let name ty expr -> do
    checkType ty
    expr <- checkExpr expr ty
    name' <- freshNameFrom name
    addScope name (ty, name')
    pure $ Let name' ty expr
  Return expr -> do
    traceM "checking return"
    case expr of
      Nothing -> do
        unifyType returnTy Void
        pure $ Return Nothing
      Just expr -> Return . Just <$> checkExpr expr returnTy
  Break -> pure Break
  _ -> undefined

checkBody :: [Stmt Parsed] -> Type -> M [Stmt Typed]
checkBody es ty = newScope do
  mapM (`checkStmt` ty) es

checkDecl :: Decl Parsed -> M (Decl Typed)
checkDecl d = do
  case d of
    Union name info@UnionInfo {unions} -> do
      checkTypeNotDuplicate name
      checkDuplicate unions \t -> "duplicate union: ".t <> t
      mapM_ checkType (fmap snd unions)
      #unions % at name .= Just info
      pure $ Union name info
    Struct name info@StructInfo {fields} -> do
      checkTypeNotDuplicate name
      checkDuplicate fields \t -> "duplicate field: ".t <> t
      mapM_ checkType (fmap snd fields)
      #structs % at name .= Just info
      pure $ Struct name info
    Enum name info@EnumInfo {tags} -> do
      checkTypeNotDuplicate name
      checkDuplicate tags \t -> "duplicate tag: ".t <> t
      #enums % at name .= Just info
      pure $ Enum name info
    Fn name FnInfo {fnType = fnType@FnType {params = paramTypes, returnType}, params, body} -> do
      traceShowM "checking fn"
      mapM_ checkType (fmap snd paramTypes)
      checkType returnType
      freshParams <- for params \param -> do
        freshNameFrom param
      body <- newScope do
        for_ (zip paramTypes freshParams) \((param, ty), param') -> do
          addScope param (ty, param')
        body <- case body of
          Nothing -> pure Nothing
          Just body -> Just <$> checkBody body returnType
        pure body
      #fns % at name .= Just fnType
      pure $ Fn name (FnInfo fnType freshParams body)
    SpannedDecl d pos -> do
      #pos .= pos
      checkDecl d

data ProgramInfo = ProgramInfo
  { structs :: HashMap Text (StructInfo),
    enums :: HashMap Text (EnumInfo),
    unions :: HashMap Text (UnionInfo),
    fns :: HashMap Text FnType
  }

checkProgram :: Program Parsed -> Either [CheckError] (Program Typed, ProgramInfo)
checkProgram Program {decls} =
  case st.errors of
    [] -> Right (res, info)
    errors -> Left errors
  where
    info = ProgramInfo {structs = st.structs, enums = st.enums, unions = st.unions, fns = st.fns}
    (res, st) = runState (runReaderT act mkCheckEnv) mkCheckState
    act = do
      decls <- for decls \decl -> do
        #unique .= 0
        checkDecl decl
      pure Program {decls}
