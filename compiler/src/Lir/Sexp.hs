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

pTyped :: SP.SParser Text
pTyped = SP.list do
  name <- SP.item SP.ident
  SP.item pType
  pure name

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

pInstr :: SP.SParser (Lir.SomeInstr Lir.Operand)
pInstr = SP.list do
  instrName <- SP.item SP.ident
  case instrName of
    "let" -> do
      name <- SP.item pTyped
      instr <- SP.item pOpInstr
      pure $ Lir.SomeInstr $ Lir.Assign (Cfg.nameFromText name) instr
    "ret" -> do
      pure $ Lir.SomeInstr $ Lir.Control Lir.Ret
    _ -> throwError (T.pack "invalid instruction")

pBlock :: SP.SParser (Cfg.Label, Lir.Block)
pBlock = SP.list do
  SP.item $ SP.lit "label"
  label <- SP.item SP.ident
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
  pure (Cfg.Label (Cfg.nameFromText label), Cfg.Block (Lir.BlockArgs []) instrs lastInstr)

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
  pure Lir.Function {name = name.str, params = map (.str) params, graph}

initLast :: [a] -> Maybe ([a], a)
initLast (x : []) = Just ([], x)
initLast (x : xs) = do
  (xs, last) <- initLast xs
  pure (x : xs, last)
initLast [] = Nothing
