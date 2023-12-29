{-# LANGUAGE ImpredicativeTypes #-}

module Cfg.Dataflow
  ( Transfer (..),
    TransferInstr (..),
    Direction,
    runTransfer,
    livenessTransfer,
    livenessInstrTransfer,
  )
where

import Cfg.Graph
import Cfg.Types
import Data.HashMap.Strict qualified as HM
import Data.HashSetDeque (HashSetDeque)
import Data.HashSetDeque qualified as HashSetDeque
import Data.Set qualified as Set
import Data.Some (Some (..))
import Imports

-- A transfer terminates if the transfer function is monotone and the domain is finite.
-- This transfer works on block granularity
data Transfer i d = Transfer
  { -- a transfer is determined solely by the predecessor facts and the current block
    -- usually a function will combine the predecessor facts first and then use it
    -- with the current block to compute the new fact
    -- it is enough to compare the sizes of the current fact and the new fact to
    -- determine if the new fact has changed this is because the transfer function is monotone
    transfer ::
      -- current block
      Block i ->
      -- predecessor/successor facts
      [d] ->
      -- current fact
      d ->
      -- returns a new fact only it is different from the current fact
      Maybe d,
    empty :: d,
    direction :: Direction
  }

data TransferInstr i d = TransferInstr
  { transfer ::
      forall c.
      -- current instruction
      i c ->
      -- previous fact
      d ->
      -- new fact
      d,
    changed ::
      -- current fact
      d ->
      -- new fact
      d ->
      Bool,
    -- empty fact, lattice bottom or top
    empty :: d,
    -- combine facts, lattice join or meet operator
    combine :: [d] -> d,
    direction :: Direction
  }

transferInstr :: TransferInstr i d -> Transfer i d
transferInstr transferInstr@TransferInstr {transfer = transferFn} =
  Transfer
    { transfer,
      empty = transferInstr.empty,
      direction = transferInstr.direction
    }
  where
    transfer block otherFacts currentFact =
      if transferInstr.changed currentFact newFacts
        then Just newFacts
        else Nothing
      where
        newFacts =
          foldlOf'
            ( case transferInstr.direction of
                Forward -> blockInstrsForward
                Backward -> blockInstrsBackward
            )
            (\prevFacts (Some instr) -> transferFn instr prevFacts)
            (transferInstr.combine otherFacts)
            block

data Direction = Forward | Backward

runTransfer :: forall i d. (HasJumps (i C)) => Transfer i d -> Graph i -> LabelMap d
runTransfer transfer graph = go initialFacts initialQueue
  where
    initialFacts = transfer.empty <$ graph.blocks
    initialQueue = HashSetDeque.fromList case transfer.direction of
      Forward -> [graph.start]
      Backward -> [graph.end]
    transpose = graphPrecessors graph
    go :: LabelMap d -> HashSetDeque Label -> LabelMap d
    go !factBase !queue = case HashSetDeque.uncons queue of
      Just (l, queue) -> case maybeNewFacts of
        Just newFact -> go (factBase & at' l ?~ newFact) newQueue
        Nothing -> do
          -- the facts for this label didn't change, so don't add its successors/predecessors to the queue
          go factBase queue
        where
          newQueue =
            foldl'
              HashSetDeque.snoc
              queue
              ( case transfer.direction of
                  Forward -> succs
                  Backward -> preds
              )
          maybeNewFacts = transfer.transfer currentBlock otherFacts currentFact
          otherFacts = case transfer.direction of
            Forward -> predFacts
            Backward -> succFacts
          currentBlock = graph.blocks ^?! ix l
          currentFact = factBase ^?! ix l
          predFacts = (\l -> factBase ^?! ix l) <$> preds
          -- some labels may not be in transpose (for example start label)
          preds = transpose ^. at l % unwrapOr []
          succFacts = (\l -> factBase ^?! ix l) <$> succs
          succs = jumps currentBlock.exit
      Nothing -> factBase
{-# INLINEABLE runTransfer #-}

type LivenessConstraint i n = (Ord n, forall c. HasDefs (i c) n, forall c. HasUses (i c) n)

livenessTransfer :: (LivenessConstraint i n) => Transfer i (Set n)
livenessTransfer = transferInstr livenessInstrTransfer
{-# INLINEABLE livenessTransfer #-}

livenessInstrTransfer :: (LivenessConstraint i n) => TransferInstr i (Set n)
livenessInstrTransfer =
  TransferInstr
    { transfer = \instr prevFacts ->
        (prevFacts `Set.difference` (Set.fromList (defs instr)))
          `Set.union` (Set.fromList (uses instr)),
      changed = \currentFact newFact -> Set.size newFact > Set.size currentFact,
      empty = Set.empty,
      combine = Set.unions,
      direction = Backward
    }
{-# INLINEABLE livenessInstrTransfer #-}
