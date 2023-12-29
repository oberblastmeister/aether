{-# LANGUAGE OverloadedStrings #-}

module Lir.Sexp (parseLir) where

import Cfg qualified
import Control.Monad.Except (MonadError (..))
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Imports
import Lir.Instr qualified as Lir
import Sexp.Parser qualified as SP
import Sexp.Syntax qualified

type SParser a = SP.SParser Identity a

type SListParser a = SP.SListParser Identity a

pTy :: SParser Lir.Ty
pTy = SP.atom \case
  "u64" -> pure Lir.TyU64
  "u1" -> pure Lir.TyU1
  _ -> throwError "invalid type"

pTyped :: SParser Lir.Value
pTyped = SP.list do
  name <- SP.item pVar
  ty <- SP.item pTy
  pure $ Lir.Value {name, ty}

pVar :: SParser Cfg.Name
pVar sexp = do
  name <- SP.ident sexp
  pure (Cfg.nameFromText name)

pLabel :: SParser Cfg.Label
pLabel sexp = Cfg.Label <$> pVar sexp

pOpInstr :: SParser (Lir.InstrOp Cfg.Name)
pOpInstr = SP.list do
  name <- SP.item SP.ident
  case name of
    "val" -> do
      ty <- SP.item pTy
      op <- SP.item pVar
      pure $ Lir.Val ty op
    "const" -> do
      ty <- SP.item pTy
      val <- SP.item $ SP.atom \t -> case TR.decimal t of
        Left e -> throwError $ "could parse number: " <> e.t
        Right (val, "") -> pure val
        Right (_, _) -> throwError "leftover characters after parsing number"
      pure $ Lir.Const ty val
    "cmp" -> do
      ty <- SP.item pTy
      op <- SP.item $ SP.atom \case
        "gt" -> pure Lir.CmpGt
        _ -> throwError "invalid comparison operator"
      op1 <- SP.item pVar
      op2 <- SP.item pVar
      pure $ Lir.Cmp ty op op1 op2
    "add" -> do
      ty <- SP.item pTy
      op1 <- SP.item pVar
      op2 <- SP.item pVar
      pure $ Lir.Add ty op1 op2
    _ -> throwError (T.pack "invalid instruction")

pBlockCall :: SParser (Lir.BlockCall Cfg.Name)
pBlockCall = SP.list do
  label <- SP.item pLabel
  args <- SP.listRest pVar
  pure $ Lir.BlockCall label args

pInstr :: SParser (Lir.SomeInstr Cfg.Name)
pInstr = SP.list do
  instrName <- SP.item SP.ident
  case instrName of
    "set" -> do
      name <- SP.item pVar
      instr <- SP.item pOpInstr
      pure $ instrO name instr
    "jump" -> do
      blockCall <- SP.item pBlockCall
      pure $ instrC $ Lir.Jump blockCall
    "cond_jump" -> do
      op <- SP.item pVar
      blockCall1 <- SP.item pBlockCall
      blockCall2 <- SP.item pBlockCall
      pure $ instrC $ Lir.CondJump op blockCall1 blockCall2
    "ret" -> do
      op <- SP.item pVar
      pure $ Lir.SomeInstr $ Lir.Control $ Lir.Ret op
    _ -> throwError (T.pack "invalid instruction")
  where
    instrO name = Lir.SomeInstr . Lir.Assign name
    instrC = Lir.SomeInstr . Lir.Control

pBlock :: SParser (Cfg.Label, Cfg.Block (Lir.Instr Cfg.Name))
pBlock = SP.list do
  SP.item $ SP.lit "label"
  (label, args) <- SP.item $ SP.list do
    label <- SP.item pVar
    args <- SP.listRest pTyped
    pure (label, args)
  instrs <- SP.listRest pInstr
  (instrs, lastInstr) <- case initLast instrs of
    Nothing -> throwError "last instruction wasn't a control instruction"
    Just x -> pure x
  instrs <- for instrs \(Lir.SomeInstr @c instr) -> do
    case Cfg.sControl @c of
      Cfg.SO -> pure instr
      _ -> throwError (T.pack "must be a non-control instruction")
  lastInstr <- case lastInstr of
    Lir.SomeInstr @c instr -> do
      case Cfg.sControl @c of
        Cfg.SC -> pure instr
        _ -> throwError (T.pack "the last instruction must be a control instruction")
  pure (Cfg.Label label, Cfg.Block (Lir.BlockArgs args) instrs lastInstr)

pGraph :: SListParser (Cfg.Graph (Lir.Instr Cfg.Name))
pGraph = do
  blocks <- SP.listRest pBlock
  case blocks of
    (label, _) : _ ->
      pure
        Cfg.Graph
          { start = label,
            blocks = HashMap.fromList blocks,
            end = blocks ^?! _last % _1
          }
    _ -> throwError "graph must have at least one block"

pFunction :: SParser (Lir.Function Cfg.Name)
pFunction = SP.list do
  SP.item $ SP.lit "define"
  (name, params) <- SP.item $ SP.list do
    name <- SP.item SP.ident
    params <- SP.listRest pTyped
    pure (name, params)
  returnTy <- SP.item pTy
  graph <- pGraph
  pure Lir.Function {name = name.str, params = params, returnTy, graph}

parseLir :: Text -> Either Text [Lir.Function Cfg.Name]
parseLir t = do
  sexp <- Sexp.Syntax.parse t
  let res = traverse (runIdentity . SP.runParser pFunction) sexp
  res & _Left %~ (T.pack . show)

initLast :: [a] -> Maybe ([a], a)
initLast (x : []) = Just ([], x)
initLast (x : xs) = do
  (xs, last) <- initLast xs
  pure (x : xs, last)
initLast [] = Nothing
