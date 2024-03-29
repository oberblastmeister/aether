{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

#include "effectful.h"

module Cp.Check
  ( ProgramInfo (..),
    checkProgram,
    checkTyped,
  )
where

import Cp.Syntax
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Str (Str)
import Data.These qualified as These
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Imports

data Scope = Scope
  { mapping :: HashMap Str (Type, LocalName)
  }

data CheckState = CheckState
  { structs :: HashMap Str StructInfo,
    enums :: HashMap Str EnumInfo,
    unions :: HashMap Str UnionInfo,
    fns :: HashMap Str FnType,
    pos :: SourcePos,
    errors :: [CheckError],
    scopes :: [Scope],
    unique :: Int,
    labels :: HashSet Str
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
      unique = 0,
      labels = mempty
    }

data CheckEnv = CheckEnv
  { inLoop :: Bool,
    variables :: HashMap Str Type,
    returnType :: Maybe Type
  }

mkCheckEnv :: CheckEnv
mkCheckEnv =
  CheckEnv
    { inLoop = False,
      variables = mempty,
      returnType = Nothing
    }

state (CheckState)
reader (CheckEnv)

checkLog :: String -> Eff es ()
-- checkLog t = do
--   pos <- use' #pos
--   traceM $ show pos ++ ": " ++ t
checkLog = const $ pure ()

newScope :: (State' :> es) => Eff es a -> Eff es a
newScope m = do
  #scopes %= (Scope {mapping = mempty} :)
  x <- m
  use' #scopes >>= \case
    _ : scopes -> do
      #scopes .= scopes
      pure x
    [] -> error "should not have empty scope"

addScope :: (State' :> es) => Str -> (Type, LocalName) -> Eff es ()
addScope name localName = #scopes % _head % #mapping % at name .= Just localName

lookupScope :: (State' :> es) => Str -> Eff es (Maybe (Type, LocalName))
lookupScope name = do
  go <$> use' #scopes
  where
    go (scope : scopes)
      | Just name' <- scope ^. #mapping % at name = Just name'
      | otherwise = go scopes
    go [] = Nothing

freshNameFrom :: (State' :> es) => Str -> Eff es LocalName
freshNameFrom name = do
  u <- use' #unique
  #unique %= (+ 1)
  pure $ LocalName name u

lookupType :: CheckState -> Str -> Bool
lookupType st n = (has (ix n) st.structs) || (has (ix n) st.enums) || (has (ix n) st.unions)

checkType :: (State' :> es) => Type -> Eff es ()
checkType ty = do
  st <- get'
  case ty of
    NamedType name -> case lookupType st name of
      True -> pure ()
      False -> checkError $ "could not find type: ".t <> name.t
    Pointer ty -> checkType ty
    _ -> pure ()

checkError :: (State' :> es) => Text -> Eff es ()
checkError error = do
  pos <- use' #pos
  #errors %= (CheckError {error, pos} :)

findDuplicate :: forall a. [(Str, a)] -> Either Str (HashMap Str a)
findDuplicate = go (Empty :: HashMap Str a)
  where
    go m ((t, x) : ts) =
      if has (ix t) m
        then Left t
        else go (m & at t .~ Just x) ts
    go m [] = Right m

checkDuplicate :: (State' :> es) => [(Str, a)] -> (Text -> Text) -> Eff es ()
checkDuplicate ts msg = case findDuplicate ts of
  Right _ -> pure ()
  Left t -> checkError (msg t.t)

eitherToM :: (State' :> es) => Either Text a -> Eff es (Maybe a)
eitherToM = \case
  Left t -> do
    checkError t
    pure Nothing
  Right a -> pure $ Just a

-- actual, expected
unifyType :: (State' :> es) => Type -> Type -> Eff es (Maybe (Expr Typed -> Expr Typed))
unifyType Unknown _ = pure Nothing
unifyType _ Unknown = pure Nothing
unifyType (PrimInt ty1@(IntType size1 signed1)) (PrimInt ty2@(IntType size2 signed2))
  | signed1 == signed2, size1 < size2 = pure $ Just \e -> Builtin $ BuiltinIntCast ty2 ty1 e
  | signed1 == signed2, size1 == size2 = pure Nothing
  | otherwise = do
      checkError $ "cannot cast from ".t <> (show ty1).t <> " to ".t <> (show ty2).t
      pure Nothing
unifyType ty1 ty2 = do
  unless (ty1 == ty2) do
    checkError $ ("expected type " <> (show ty2) <> ", got " <> (show ty1)).t
  pure Nothing

u8Ty :: Type
u8Ty = PrimInt (IntType U8 Unsigned)

expectError :: (State' :> es) => Text -> Text -> Eff es ()
expectError actual expected = checkError $ "expected ".t <> expected <> ", got ".t <> actual

maybeApply :: a -> Maybe (a -> a) -> a
maybeApply x Nothing = x
maybeApply x (Just f) = f x

checkLiteral :: (State' :> es) => Literal Parsed -> Type -> Eff es (Expr Typed)
checkLiteral lit ty = do
  checkLog "checking literal"
  case lit of
    Num lit () -> do
      case ty of
        PrimInt intTy
          | literalInBounds lit.num intTy -> do
              pure $ Literal $ Num lit intTy
          | otherwise -> do
              expectError "integer out of bounds".t (show intTy).t
              pure Error
        _ -> do
          expectError "integer".t (show ty).t
          pure Error
    _ -> do
      (ty', expr) <- inferLiteral lit
      unifyType ty' ty <&> maybeApply expr

builtinSpec :: BuiltinTag -> BuiltinSpec
builtinSpec tag = case tag of
  Add -> binNum "add".str
  Sub -> binNum "sub".str
  Mul -> binNum "mul".str
  Eq -> binNumBool "eq".str
  Lt -> binNumBool "lt".str
  Le -> binNumBool "le".str
  Ge -> binNumBool "ge".str
  Gt -> binNumBool "gt".str
  _ -> todo
  where
    binNum = bin checkIntType id 2
    binNumBool = bin checkIntType (const PrimBool) 2
    bin :: (forall es. (State' :> es) => Type -> Eff es ()) -> (Type -> Type) -> Int -> Str -> BuiltinSpec
    bin checkTy returnTy num name =
      BuiltinSpec name \args -> case args of
        (TypeArg ty) : rest | length @[] rest == num -> do
          _ <- checkTy ty
          args' <- traverseOf (each % #_ExprArg) (`checkExpr` ty) args
          pure $ Just (returnTy ty, args')
        _ -> do
          checkError $ "invalid arguments for ".t <> name.t
          pure Nothing
    checkIntType :: (State' :> es) => Type -> Eff es ()
    checkIntType ty = case ty of
      PrimInt _ -> pure ()
      _ -> checkError $ "expected int type".t

data BuiltinSpec = BuiltinSpec
  { name :: Str,
    parseArgs :: forall es. (State' :> es) => [BuiltinArg Parsed] -> Eff es (Maybe (Type, [BuiltinArg Typed]))
  }

builtinSpecs :: [(BuiltinTag, BuiltinSpec)]
builtinSpecs = (\tag -> (tag, builtinSpec tag)) <$> [minBound :: BuiltinTag .. maxBound]

inferWrong :: (Type, Expr p)
inferWrong = (Unknown, Error)

parseBuiltin :: (State' :> es) => BuiltinParsed -> Eff es (Type, Expr Typed)
parseBuiltin builtin
  | builtin.name == "cast".str,
    [TypeArg ty, ExprArg expr] <- builtin.args = do
      (ty', expr) <- inferExpr expr
      unless (isValidCastTy ty ty') do
        checkError $ "invalid cast from ".t <> (show ty').t <> " to ".t <> (show ty).t
      pure (ty, expr)
  | otherwise = do
      case findRes of
        Nothing -> do
          checkError $ "could not find builtin ".t <> builtin.name.t
          pure inferWrong
        Just (tag, BuiltinSpec {parseArgs}) -> do
          parseArgs builtin.args >>= \case
            Nothing -> do
              checkError $ "invalid arguments for ".t <> builtin.name.t
              pure inferWrong
            Just (ty, args) -> pure (ty, Builtin $ SomeBuiltin tag args)
  where
    isValidCastTy ty ty' = case (ty, ty') of
      (PrimInt _, PrimInt _) -> True
      (PrimInt _, PrimBool) -> True
      (PrimBool, PrimInt _) -> True
      (PrimBool, PrimBool) -> True
      (Pointer _, Pointer _) -> True
      _ -> False
    findRes = findOf each (\(_tag, spec) -> spec.name == builtin.name) builtinSpecs

inferExpr :: (State' :> es) => Expr Parsed -> Eff es (Type, Expr Typed)
inferExpr expr = case expr of
  Call call args -> do
    fn <- use' (#fns % at call)
    case fn of
      Nothing -> do
        checkError $ "could not find function ".t <> call.t
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
  BraceLiteral fields -> inferBraceLiteral Nothing fields
  Literal lit -> inferLiteral lit
  Var var ->
    lookupScope var >>= \case
      Nothing -> do
        checkError $ "var ".t <> var.t <> " not found".t
        pure (Unknown, Error)
      Just res -> pure $ Var <$> res
  Ref expr -> do
    (ty, expr) <- inferExpr expr
    case getLValue expr of
      Nothing -> do
        checkError $ "cannot take reference of non-lvalue".t
        pure inferWrong
      Just _lvalue -> pure (Pointer ty, Ref expr)
  Deref expr -> do
    (ty, expr) <- inferExpr expr
    case ty of
      Pointer ty -> pure (ty, Deref expr)
      _ -> do
        checkError $ "expected pointer type".t
        pure inferWrong
  GetField expr fieldName -> do
    (ty, expr) <- inferExpr expr
    st <- get'
    case ty of
      NamedType name | Just struct <- st.structs ^? ix name -> do
        case struct.fieldMap ^. at fieldName of
          Nothing -> do
            checkError $ "could not find field ".t <> (show fieldName).t <> " in struct ".t <> name.t
            pure inferWrong
          Just ty -> pure (ty, GetField expr name)
      NamedType name -> do
        checkError $ "could not find struct ".t <> name.t
        pure inferWrong
      _ -> do
        checkError $ "expected named type".t
        pure inferWrong
  Builtin builtin -> parseBuiltin builtin
  _ -> pure inferWrong

checkExpr :: (State' :> es) => Expr Parsed -> Type -> Eff es (Expr Typed)
checkExpr expr ty = do
  checkLog $ "checking expr: " ++ show expr ++ " ty: " ++ show ty
  case expr of
    Literal lit -> checkLiteral lit ty
    BraceLiteral fields -> do
      (_, res) <- (inferBraceLiteral (Just ty) fields)
      pure res
    _ -> do
      (ty', expr) <- inferExpr expr
      unifyType ty' ty <&> maybeApply expr

getStructPartial :: ((State' :> es), HasCallStack) => Type -> Eff es (StructInfo)
getStructPartial ty = do
  st <- get'
  let !struct = st.structs ^?! ix (ty ^?! #_NamedType)
  pure struct

checkFieldsOkay :: [(Str, Expr Parsed)] -> HashMap Str Type -> Either Text [(Str, Expr Parsed, Type)]
checkFieldsOkay fields fieldTypes =
  case findDuplicate fields of
    Right fieldExprs -> do
      let extraFields = fieldExprs `HM.difference` fieldTypes
      unless (has _Empty extraFields) do
        Left $ "extra fields: ".t <> (show extraFields).t
      let missingFields = fieldTypes `HM.difference` fieldExprs
      unless (has _Empty missingFields) do
        Left $ "missing fields: ".t <> (show missingFields).t
      let together = HM.intersectionWith (,) fieldExprs fieldTypes & HM.toList & fmap (\(x, (y, z)) -> (x, y, z))
      pure together
    Left name -> Left $ "duplicate field name: ".t <> (show name).t

inferBraceLiteral :: (State' :> es) => Maybe Type -> Fields Parsed -> Eff es (Type, Expr Typed)
inferBraceLiteral maybeTy fields = do
  case res of
    Nothing -> do
      void $ maybeUnify Void
      pure (Void, BraceLiteral VoidLiteral)
    Just (These.This fields) | Just ty <- maybeTy -> do
      st <- get'
      case ty of
        NamedType name | Just struct <- st.structs ^? ix name -> do
          eitherToM (checkFieldsOkay fields struct.fieldMap) >>= \case
            Nothing -> pure inferWrong
            Just fields -> do
              fields <- for fields \(name, expr, ty) -> do
                expr <- checkExpr expr ty
                pure (name, expr)
              pure (ty, BraceLiteral $ StructLiteral fields (HM.fromList fields) struct name)
        NamedType name | otherwise -> do
          checkError $ "could not find struct ".t <> name.t
          pure inferWrong
        _ -> do
          checkError "expected named type".t
          pure inferWrong
    Just (These.This _) | Nothing <- maybeTy -> do
      checkError "cannot infer type of struct literal".t
      pure inferWrong
    Just (These.That ()) -> do
      void $ maybeUnify Void
      pure (Void, BraceLiteral $ VoidLiteral)
    Just (These.These _ _) -> do
      checkError "cannot mix named and unnamed fields".t
      pure inferWrong
  where
    res =
      foldMapOf
        (each)
        (Just . \case (Nothing, _) -> These.That (); (Just name, expr) -> These.This [(name, expr)])
        fields
    maybeUnify ty' = case maybeTy of
      Nothing -> pure Nothing
      Just ty -> unifyType ty ty'

intTypeBounds :: IntType -> (Integer, Integer)
intTypeBounds (IntType size signed) = case signed of
  Unsigned -> case size of
    U8 -> (fromIntegral $ minBound @Word8, fromIntegral $ maxBound @Word8)
    U16 -> (fromIntegral $ minBound @Word16, fromIntegral $ maxBound @Word16)
    U32 -> (fromIntegral $ minBound @Word32, fromIntegral $ maxBound @Word32)
    U64 -> (fromIntegral $ minBound @Word64, fromIntegral $ maxBound @Word64)
  Signed -> case size of
    U8 -> (fromIntegral $ minBound @Int8, fromIntegral $ maxBound @Int8)
    U16 -> (fromIntegral $ minBound @Int16, fromIntegral $ maxBound @Int16)
    U32 -> (fromIntegral $ minBound @Int32, fromIntegral $ maxBound @Int32)
    U64 -> (fromIntegral $ minBound @Int64, fromIntegral $ maxBound @Int64)

unsignedBounds :: [(Size, Integer, Integer)]
unsignedBounds = (\size -> let (lower, upper) = intTypeBounds (IntType size Unsigned) in (size, lower, upper)) <$> [U8 .. U64]

signedBounds :: [(Size, Integer, Integer)]
signedBounds = (\size -> let (lower, upper) = intTypeBounds (IntType size Signed) in (size, lower, upper)) <$> [U8 .. U64]

literalInBounds :: Integer -> IntType -> Bool
literalInBounds i (IntType size signed) = low <= i && i <= high
  where
    (low, high) = intTypeBounds (IntType size signed)

inferLiteral :: (State' :> es) => Literal Parsed -> Eff es (Type, Expr Typed)
inferLiteral lit =
  case lit of
    String s -> pure $ (Pointer u8Ty, Literal $ String s)
    Char c -> pure $ (u8Ty, Literal $ Char c)
    Num num () -> do
      let i = num.num
      case i < 0 of
        True -> do
          let signed = findOf each (\(_size, min, max) -> i >= min && i <= max) signedBounds
          case signed of
            Nothing -> do
              checkError $ "integer out of bounds".t
              pure inferWrong
            Just (size, _, _) -> do
              let ty = IntType size Signed
              pure (PrimInt ty, Literal $ Num num ty)
        False -> do
          let unsigned = findOf each (\(_size, min, max) -> i >= min && i <= max) unsignedBounds
          case unsigned of
            Nothing -> do
              checkError $ "integer out of bounds".t
              pure inferWrong
            Just (size, _, _) -> do
              let ty = IntType size Unsigned
              pure (PrimInt ty, Literal $ Num num ty)
    Bool b -> pure (PrimBool, Literal $ Bool b)

checkTypeNotDuplicate :: (State' :> es) => Str -> Eff es ()
checkTypeNotDuplicate name = do
  st <- get'
  checkLog $ "st: " ++ (pShowC st.structs).s
  case lookupType st name of
    True -> checkError $ ("duplicate type: ".t <> name.t)
    False -> pure ()

checkCont :: (State' :> es) => IfCont Parsed -> Type -> Eff es (IfCont Typed)
checkCont ic ty = case ic of
  ElseIf {cond, body, cont} -> do
    cond <- checkExpr cond PrimBool
    body <- checkBody body ty
    cont <- checkCont cont ty
    pure ElseIf {cond, body, cont}
  Else {body} -> do
    body <- checkBody body ty
    pure Else {body}
  NoIfCont -> pure NoIfCont

-- the type is the type of the return
checkStmt :: (State' :> es) => Stmt Parsed -> Type -> Eff es (Stmt Typed)
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
  Let name (Just ty) expr () -> do
    checkType ty
    expr <- checkExpr expr ty
    name' <- freshNameFrom name
    addScope name (ty, name')
    pure $ Let name' (Just ty) expr ty
  Let name Nothing expr () -> do
    (ty, expr) <- inferExpr expr
    name' <- freshNameFrom name
    addScope name (ty, name')
    pure $ Let name' (Just ty) expr ty
  Return expr -> do
    checkLog "checking return"
    case expr of
      Nothing -> do
        void $ unifyType returnTy Void
        pure $ Return Nothing
      Just expr -> Return . Just <$> checkExpr expr returnTy
  Break -> pure Break
  Set _ _ -> todo
  Goto label -> do
    labels <- use' #labels
    unless (labels ^. contains label) do
      checkError $ "could not find label ".t <> label.t
    pure $ Goto label
  Label label -> do
    labels <- use' #labels
    when (labels ^. contains label) do
      checkError $ "duplicate label ".t <> label.t
    #labels %= (label `HS.insert`)
    pure $ Label label

checkBody :: (State' :> es) => [Stmt Parsed] -> Type -> Eff es [Stmt Typed]
checkBody es ty = newScope do
  mapM (`checkStmt` ty) es

checkDecl :: (State' :> es) => Decl Parsed -> Eff es (Decl Typed)
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
      checkLog "checking fn"
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

checkProgram :: ProgramParsed -> Either [CheckError] (ProgramTyped)
checkProgram ProgramParsed {decls = declsParsed} =
  case st.errors of
    [] -> Right ProgramTyped {decls, info}
    errors -> Left errors
  where
    info = ProgramInfo {structs = st.structs, enums = st.enums, unions = st.unions, fns = st.fns}
    (decls, st) = runPureEff (runState mkCheckState (runReader mkCheckEnv act))
    act = do
      decls <- for declsParsed \decl -> do
        #unique .= 0
        #labels .= mempty
        checkDecl decl
      pure decls

checkTyped :: ProgramTyped -> Either [CheckError] ProgramTyped
checkTyped = checkProgram . revertProgram

revertProgram :: ProgramTyped -> ProgramParsed
revertProgram ProgramTyped {decls} = ProgramParsed (revertDecl <$> decls)

revertDecl :: Decl Typed -> Decl Parsed
revertDecl decl = case decl of
  Struct name info -> Struct name info
  Fn name info -> Fn name (FnInfo info.fnType (revertVar <$> info.params) (revertBody <$> info.body))
  Enum name info -> Enum name info
  Union name info -> Union name info
  SpannedDecl decl pos -> SpannedDecl (revertDecl decl) pos

revertVar :: Var Typed -> Var Parsed
revertVar v = (localNameToText v).str

revertBody :: [Stmt Typed] -> [Stmt Parsed]
revertBody stmt = revertStmt <$> stmt

revertStmt :: Stmt Typed -> Stmt Parsed
revertStmt stmt = case stmt of
  If cond body cont -> If (revertExpr cond) (revertBody body) (revertIfCont cont)
  Loop body -> Loop $ revertBody body
  ExprStmt expr -> ExprStmt $ revertExpr expr
  Let var ty expr _ -> Let (revertVar var) ty (revertExpr expr) ()
  Set lvalue expr' -> Set (revertLValue lvalue) (revertExpr expr')
  Return expr -> Return (revertExpr <$> expr)
  Break -> Break
  Goto name -> Goto name
  Label name -> Label name

revertLValue :: LValue -> Expr Parsed
revertLValue lvalue = case lvalue of
  LValueDeref expr -> Deref $ revertExpr expr
  LValueVar var -> Var $ revertVar var
  LValueGetField lvalue name -> GetField (revertLValue lvalue) name

revertIfCont :: IfCont Typed -> IfCont Parsed
revertIfCont cont = case cont of
  ElseIf cond body cont -> ElseIf (revertExpr cond) (revertBody body) (revertIfCont cont)
  Else body -> Else (revertBody body)
  NoIfCont -> NoIfCont

revertExpr :: Expr Typed -> Expr Parsed
revertExpr expr = case expr of
  Deref expr -> Deref $ revertExpr expr
  GetField expr name -> GetField (revertExpr expr) name
  As ty expr -> As ty (revertExpr expr)
  Call var exprs -> do
    let var' = case var of
          CallTop name -> name
          CallLocal name -> revertVar name
    Call var' (revertExpr <$> exprs)
  Builtin builtin ->
    case builtin of
      SomeBuiltin tag args -> do
        let args' = args & each % #_ExprArg %~ revertExpr
        case findOf each ((== tag) . fst) builtinSpecs of
          Nothing -> Builtin $ BuiltinParsed "__could_not_find_tag_name".str args'
          Just (_tag, spec) -> Builtin $ BuiltinParsed spec.name args'
      BuiltinCast ty ty' expr -> Builtin $ BuiltinParsed "cast".str [TypeArg ty, ExprArg (As ty' $ revertExpr expr)]
      BuiltinIntCast ty _ty' expr -> As (PrimInt ty) (revertExpr expr)
  SpannedExpr expr p -> SpannedExpr (revertExpr expr) p
  NullPtr -> NullPtr
  Ref expr -> Ref $ revertExpr expr
  Literal lit -> case lit of
    String s -> Literal $ String s
    Char c -> Literal $ Char c
    Num num ty -> As (PrimInt ty) $ Literal (Num num ())
    Bool b -> Literal $ Bool b
  Error -> Error
  EnumLiteral name _ -> EnumLiteral name ()
  Var var -> Var $ revertVar var
  BraceLiteral (ArrayLiteral exprs) -> BraceLiteral (fmap ((Nothing,) . revertExpr) exprs)
  BraceLiteral (StructLiteral fields _map _info name) -> As (NamedType name) (BraceLiteral (fmap (\(name, expr) -> (Just name, revertExpr expr)) fields))
  BraceLiteral VoidLiteral -> As Void (BraceLiteral [])

getLValue :: Expr Typed -> Maybe LValue
getLValue (Var p) = Just $ LValueVar p
getLValue (Deref expr) = Just $ LValueDeref expr
getLValue (GetField expr name) = LValueGetField <$> (getLValue expr) <*> pure name
getLValue _ = Nothing
