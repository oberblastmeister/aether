{-# LANGUAGE OverloadedStrings #-}

module Lir.Pretty
  ( functionToText,
    pFunction,
  )
where

import Cfg (Control (..))
import Cfg qualified
import Data.List qualified as List
import Imports
import Lir.Instr qualified as Lir
import Sexp.Syntax

functionToText :: Lir.Function Lir.Value -> Text
functionToText = prettyText . pFunction

pFunction :: Lir.Function Lir.Value -> SexpP
pFunction fn =
  ListP
    ( [ "define",
        ListP ([AtomP fn.name.t] ++ (pValue <$> fn.params)),
        pTy fn.returnTy,
        AnnP IndentLine
      ]
        ++ pGraph fn.graph
    )

pGraph :: Lir.Graph -> [SexpP]
pGraph graph =
  [pBlock graph.start startBlock]
    ++ [AnnP Line]
    ++ endWith [AnnP Line] (itoList blocks & List.sortOn fst <&> uncurry pBlock)
    ++ [pBlock graph.end endBlock]
  where
    startBlock :: Lir.Block = graph.blocks ^?! ix graph.start
    endBlock = graph.blocks ^?! ix graph.end
    blocks :: Cfg.LabelMap Lir.Block = graph.blocks & at graph.start .~ Nothing & at graph.end .~ Nothing

endWith :: [a] -> [a] -> [a]
endWith xs ys = concat $ map (\y -> [y] ++ xs) ys

pBlock :: Cfg.Label -> Lir.Block -> SexpP
pBlock label block =
  ListP
    ( [ "label",
        ListP
          ( [AtomP (Cfg.showName label.name)]
              ++ (pValue <$> (case block.entry of Lir.BlockArgs args -> args))
          ),
        AnnP IndentLine
      ]
        ++ endWith [AnnP Line] (fmap pInstrO block.body)
        ++ [pInstrC block.exit]
    )

pInstrO :: Lir.Instr Lir.Value O -> SexpP
pInstrO (val Lir.:= instr) =
  ListP
    [ "set",
      pValue val,
      pInstrOp instr
    ]

pInstrOp :: Lir.InstrOp Lir.Value -> SexpP
pInstrOp instr = case instr of
  Lir.Add ty v1 v2 -> ListP ["add", pTy ty, pValueName v1, pValueName v2]
  Lir.Sub ty v1 v2 -> ListP ["sub", pTy ty, pValueName v1, pValueName v2]
  Lir.Call ty name args -> ListP ["call", pTy ty, AtomP name.t, ListP (pValueName <$> args)]
  Lir.Const ty val -> ListP ["const", pTy ty, AtomP (show val).t]
  Lir.Cmp ty v v1 v2 -> ListP ["cmp", pTy ty, pCmpOp v, pValueName v1, pValueName v2]
  Lir.Val ty v -> ListP ["val", pTy ty, pValueName v]

pInstrC :: Lir.Instr Lir.Value C -> SexpP
pInstrC (Lir.Control instr) = pInstrControl instr

pInstrControl :: Lir.InstrControl Lir.Value -> SexpP
pInstrControl instr = case instr of
  Lir.Jump blockCall -> ListP ["jump", pBlockCall blockCall]
  Lir.CondJump v blockCall1 blockCall2 -> ListP ["cond_jump", pValueName v, pBlockCall blockCall1, pBlockCall blockCall2]
  Lir.Ret v -> ListP ["ret", pValueName v]

pBlockCall :: Lir.BlockCall Lir.Value -> SexpP
pBlockCall blockCall = ListP ([AtomP (Cfg.showName blockCall.label.name)] ++ fmap pValueName blockCall.args)

pCmpOp :: Lir.CmpOp -> SexpP
pCmpOp op = case op of
  Lir.CmpGt -> "gt"

pTy :: Lir.Ty -> SexpP
pTy ty = case ty of
  Lir.TyU1 -> "u1"
  Lir.TyU64 -> "u64"

pValueName :: Lir.Value -> SexpP
pValueName (Lir.Value name _ty) = AtomP (Cfg.showName name)

pValue :: Lir.Value -> SexpP
pValue (Lir.Value name ty) = ListP [AtomP (Cfg.showName name), pTy ty]
