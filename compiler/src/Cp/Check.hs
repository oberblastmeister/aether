{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cp.Check
  ( ProgramInfo (..),
    checkProgram,
    checkTyped,
  )
where

import Cp.Syntax
import Data.HashMap.Strict qualified as HM
import Data.These qualified as These
import Imports

data Scope = Scope
  { mapping :: HashMap Text (Type, LocalName)
  }

data CheckState = CheckState
  { structs :: HashMap Text StructInfo,
    enums :: HashMap Text EnumInfo,
    unions :: HashMap Text UnionInfo,
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

eitherToM :: Either Text a -> M (Maybe a)
eitherToM = \case
  Left t -> do
    checkError t
    pure Nothing
  Right a -> pure $ Just a

-- actual, expected
unifyType :: Type -> Type -> M (Maybe (Expr Typed -> Expr Typed))
unifyType Unknown _ = pure Nothing
unifyType _ Unknown = pure Nothing
unifyType (PrimInt ty1@(IntType size1 signed1)) (PrimInt ty2@(IntType size2 signed2))
  | signed1 == signed2, size1 < size2 = pure $ Just \e -> IntCast ty2 ty1 e ()
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

expectError :: Text -> Text -> M ()
expectError actual expected = checkError $ "expected ".t <> expected <> ", got ".t <> actual

maybeApply :: a -> Maybe (a -> a) -> a
maybeApply x Nothing = x
maybeApply x (Just f) = f x

checkLiteral :: Literal Parsed -> Type -> M (Expr Typed)
checkLiteral lit ty = do
  traceM "checking literal"
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

builtinSpec :: BuiltinTag -> BuiltinSpec
builtinSpec tag = case tag of
  Add -> binNum "add".t
  Sub -> binNum "sub".t
  Mul -> binNum "mul".t
  Eq -> binNumBool "eq".t
  Lt -> binNumBool "lt".t
  Le -> binNumBool "le".t
  Ge -> binNumBool "ge".t
  Gt -> binNumBool "gt".t
  _ -> todo
  where
    binNum = bin checkIntType id 2
    binNumBool = bin checkIntType (const PrimBool) 2
    bin checkTy returnTy num name =
      BuiltinSpec name \args -> case args of
        (Left ty) : rest | length @[] rest == num -> do
          _ <- checkTy ty
          args' <- traverseOf (each % _Right) (`checkExpr` ty) args
          pure $ Just (returnTy ty, args')
        _ -> do
          checkError $ "invalid arguments for ".t <> name
          pure Nothing
    checkIntType ty = case ty of
      PrimInt _ -> pure ()
      _ -> checkError $ "expected int type".t

data BuiltinSpec = BuiltinSpec
  { name :: Text,
    parseArgs :: BuiltinArgs Parsed -> M (Maybe (Type, BuiltinArgs Typed))
  }

builtinSpecs :: [(BuiltinTag, BuiltinSpec)]
builtinSpecs = (\tag -> (tag, builtinSpec tag)) <$> [minBound :: BuiltinTag .. maxBound]

inferWrong :: (Type, Expr p)
inferWrong = (Unknown, Error)

parseBuiltin :: BuiltinParsed -> M (Type, Expr Typed)
parseBuiltin builtin
  | builtin.name == "cast".t,
    [Left ty, Right expr] <- builtin.args = do
      (ty', expr) <- inferExpr expr
      unless (isValidCastTy ty ty') do
        checkError $ "invalid cast from ".t <> (show ty').t <> " to ".t <> (show ty).t
      pure (ty, expr)
  | otherwise = do
      case findRes of
        Nothing -> do
          checkError $ "could not find builtin ".t <> builtin.name
          pure inferWrong
        Just (tag, spec) -> do
          spec.parseArgs builtin.args >>= \case
            Nothing -> do
              checkError $ "invalid arguments for ".t <> builtin.name
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
  BraceLiteral fields _ -> inferBraceLiteral Nothing fields
  Literal lit -> inferLiteral lit
  Var var ->
    lookupScope var >>= \case
      Nothing -> do
        checkError $ "var ".t <> var <> " not found".t
        pure (Unknown, Error)
      Just res -> pure $ Var <$> res
  Ref expr -> do
    todo
  PlaceExpr place -> (fmap . fmap) PlaceExpr (inferPlace place)
  Builtin builtin -> parseBuiltin builtin
  _ -> pure inferWrong

checkExpr :: Expr Parsed -> Type -> M (Expr Typed)
checkExpr expr ty = do
  traceM $ "checking expr: " ++ show expr ++ " ty: " ++ show ty
  case expr of
    Literal lit -> checkLiteral lit ty
    PlaceExpr (Place expr place) -> do
      case place of
        PlaceHole -> checkExpr expr ty
        _ -> todo
    BraceLiteral fields () -> do
      (_, res) <- (inferBraceLiteral (Just ty) fields)
      pure res
    _ -> do
      (ty', expr) <- inferExpr expr
      unifyType ty' ty <&> maybeApply expr

getStructPartial :: (HasCallStack) => Type -> M (StructInfo)
getStructPartial ty = do
  st <- get
  let !struct = st.structs ^?! ix (ty ^?! #_NamedType)
  pure struct

checkFieldsOkay :: [(Text, Expr Parsed)] -> HashMap Text Type -> Either Text [(Text, Expr Parsed, Type)]
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

inferBraceLiteral :: Maybe Type -> Fields Parsed -> M (Type, Expr Typed)
inferBraceLiteral maybeTy fields = do
  case res of
    Nothing -> do
      void $ maybeUnify Void
      pure (Void, VoidLiteral ())
    Just (These.This fields) | Just ty <- maybeTy -> do
      st <- get
      case ty of
        NamedType name | Just struct <- st.structs ^? ix name -> do
          eitherToM (checkFieldsOkay fields struct.fieldMap) >>= \case
            Nothing -> pure inferWrong
            Just fields -> do
              fields <- for fields \(name, expr, ty) -> do
                expr <- checkExpr expr ty
                pure (name, expr)
              pure (ty, StructLiteral fields (HM.fromList fields) struct name ())
        NamedType name | otherwise -> do
          checkError $ "could not find struct ".t <> name
          pure inferWrong
        _ -> do
          checkError "expected named type".t
          pure inferWrong
    Just (These.This _) | Nothing <- maybeTy -> do
      checkError "cannot infer type of struct literal".t
      pure inferWrong
    Just (These.That ()) -> do
      void $ maybeUnify Void
      pure (Void, VoidLiteral ())
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

inferLiteral :: Literal Parsed -> M (Type, Expr Typed)
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

checkTypeNotDuplicate :: Text -> M ()
checkTypeNotDuplicate name = do
  st <- get
  traceM $ "st: " ++ show st.structs
  case lookupType st name of
    True -> checkError $ ("duplicate type: ".t <> name)
    False -> pure ()

checkCont :: IfCont Parsed -> Type -> M (IfCont Typed)
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
    traceM "checking return"
    case expr of
      Nothing -> do
        void $ unifyType returnTy Void
        pure $ Return Nothing
      Just expr -> Return . Just <$> checkExpr expr returnTy
  Break -> pure Break
  Set _ _ -> todo

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
      traceM "checking fn"
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
    (decls, st) = runState (runReaderT act mkCheckEnv) mkCheckState
    act = do
      decls <- for declsParsed \decl -> do
        #unique .= 0
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
revertVar = localNameToText

revertBody :: [Stmt Typed] -> [Stmt Parsed]
revertBody stmt = revertStmt <$> stmt

revertStmt :: Stmt Typed -> Stmt Parsed
revertStmt stmt = case stmt of
  If cond body cont -> If (revertExpr cond) (revertBody body) (revertIfCont cont)
  Loop body -> Loop $ revertBody body
  ExprStmt expr -> ExprStmt $ revertExpr expr
  Let var ty expr _ -> Let (revertVar var) ty (revertExpr expr) ()
  Set place expr -> Set (revertPlace place) (revertExpr expr)
  Return expr -> Return (revertExpr <$> expr)
  Break -> Break

revertIfCont :: IfCont Typed -> IfCont Parsed
revertIfCont cont = case cont of
  ElseIf cond body cont -> ElseIf (revertExpr cond) (revertBody body) (revertIfCont cont)
  Else body -> Else (revertBody body)
  NoIfCont -> NoIfCont

revertPlace :: Place Typed -> Place Parsed
revertPlace (Place expr cx) = Place (revertExpr expr) cx

revertExpr :: Expr Typed -> Expr Parsed
revertExpr expr = case expr of
  As ty expr -> As ty (revertExpr expr)
  Call var exprs -> do
    let var' = case var of
          CallTop name -> name
          CallLocal name -> revertVar name
    Call var' (revertExpr <$> exprs)
  Builtin builtin ->
    Builtin case builtin of
      SomeBuiltin tag args -> do
        let args' = args & each % _Right %~ revertExpr
        case findOf each ((== tag) . fst) builtinSpecs of
          Nothing -> BuiltinParsed "__could_not_find_tag_name".t args'
          Just (_tag, spec) -> BuiltinParsed spec.name args'
  SpannedExpr expr p -> SpannedExpr (revertExpr expr) p
  Null -> Null
  PlaceExpr place -> PlaceExpr $ revertPlace place
  Ref expr -> Ref $ revertExpr expr
  Literal lit -> case lit of
    String s -> Literal $ String s
    Char c -> Literal $ Char c
    Num num ty -> As (PrimInt ty) $ Literal (Num num ())
    Bool b -> Literal $ Bool b
  Error -> Error
  EnumLiteral name _ -> EnumLiteral name ()
  Var var -> Var $ revertVar var
  Cast ty ty' expr -> Cast ty ty' (revertExpr expr)
  IntCast ty _ty' expr () -> As (PrimInt ty) (revertExpr expr)
  ArrayLiteral exprs () -> BraceLiteral (fmap ((Nothing,) . revertExpr) exprs) ()
  StructLiteral fields _map _info name () -> As (NamedType name) (BraceLiteral (fmap (\(name, expr) -> (Just name, revertExpr expr)) fields) ())
  VoidLiteral () -> BraceLiteral [] ()
