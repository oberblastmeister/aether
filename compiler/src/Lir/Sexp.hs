{-# LANGUAGE OverloadedStrings #-}

module Lir.Sexp where

import Cfg qualified
import Control.Monad.Except (MonadError (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Imports
import Lir.Instr qualified as Lir
import Sexp.Parser qualified as SP

pType :: SP.SParser ()
pType = SP.lit "u64"

pTyped :: SP.SParser Cfg.Name
pTyped = SP.list do
  name <- SP.item pVar
  SP.item pType
  pure name

pVar :: SP.SParser Cfg.Name
pVar sexp = do
  name <- SP.ident sexp
  pure (Cfg.nameFromText name)

pLabel :: SP.SParser Cfg.Label
pLabel sexp = Cfg.Label <$> pVar sexp

pOperand :: SP.SParser Lir.Operand
pOperand sexp = do
  name <- SP.ident sexp
  pure (Lir.mkVar name)

pOpInstr :: SP.SParser (Lir.OpInstr Lir.Operand)
pOpInstr = SP.list do
  name <- SP.item SP.ident
  case name of
    "add" -> do
      _ty <- SP.item pType
      op1 <- SP.item pOperand
      op2 <- SP.item pOperand
      pure $ Lir.Add op1 op2
    _ -> throwError (T.pack "invalid instruction")

pBlockCall :: SP.SParser (Lir.BlockCall Lir.Operand)
pBlockCall = SP.list do
  label <- SP.item pLabel
  args <- SP.listRest pOperand
  pure $ Lir.BlockCall label args

pInstr :: SP.SParser (Lir.SomeInstr Lir.Operand)
pInstr = SP.list do
  instrName <- SP.item SP.ident
  case instrName of
    "let" -> do
      name <- SP.item pTyped
      instr <- SP.item pOpInstr
      pure $ instrO name instr
    "jump" -> do
      blockCall <- SP.item pBlockCall
      pure $ instrC $ Lir.Jump blockCall
    "cond_jump" -> do
      op <- SP.item pOperand
      blockCall1 <- SP.item pBlockCall
      blockCall2 <- SP.item pBlockCall
      pure $ instrC $ Lir.CondJump op blockCall1 blockCall2
    "ret" -> do
      pure $ Lir.SomeInstr $ Lir.Control Lir.Ret
    _ -> throwError (T.pack "invalid instruction")
  where
    instrO name = Lir.SomeInstr . Lir.Assign name
    instrC = Lir.SomeInstr . Lir.Control

pBlock :: SP.SParser (Cfg.Label, Lir.Block)
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

pGraph :: SP.SListParser Lir.Graph
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

pFunction :: SP.SParser Lir.Function
pFunction = SP.list do
  SP.item $ SP.lit "define"
  (name, params) <- SP.item $ SP.list do
    name <- SP.item SP.ident
    params <- SP.listRest pTyped
    pure (name, params)
  SP.item pType
  graph <- pGraph
  pure Lir.Function {name = name.str, params = params, graph}

initLast :: [a] -> Maybe ([a], a)
initLast (x : []) = Just ([], x)
initLast (x : xs) = do
  (xs, last) <- initLast xs
  pure (x : xs, last)
initLast [] = Nothing
