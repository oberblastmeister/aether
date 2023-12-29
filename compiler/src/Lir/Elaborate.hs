module Lir.Elaborate
  ( elabFunction,
  )
where

import Cfg qualified
import Control.Monad.Except
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HM
import Data.Some (Some (..))
import Imports
import Lir.Instr qualified as Lir

type TyMap = HashMap Cfg.Name Lir.Ty

type M = ExceptT Text (Reader TyMap)

collectTypes :: Lir.Function Cfg.Name -> Either Text TyMap
collectTypes fn = res
  where
    res = itraverseOf each findRep nameToTyList

    findRep :: Cfg.Name -> NonEmpty Lir.Ty -> Either Text Lir.Ty
    findRep name (ty :| tys) = case all (== ty) tys of
      True -> pure ty
      False -> Left $ "types do not match for variable ".t <> Cfg.nameText name

    nameToTyList =
      HM.fromListWith (<>) $
        foldMapOf
          (#graph % #blocks % each % Cfg.blockInstrsForward)
          (\(Some instr) -> collect instr)
          fn

    collect :: Lir.Instr Cfg.Name c -> [(Cfg.Name, NonEmpty Lir.Ty)]
    collect (Lir.Assign name op) = [(name, Lir.getOpInstrTy op :| [])]
    collect _ = []

elabFunction :: Lir.Function Cfg.Name -> Either Text (Lir.Function Lir.Value)
elabFunction fn = do
  tyMap <- collectTypes fn
  runReader (runExceptT (elabFunction' fn)) tyMap

elabFunction' :: Lir.Function Cfg.Name -> M (Lir.Function Lir.Value)
elabFunction' = traverseOf (#graph % #blocks % each) elabBlock

lookupTy :: Cfg.Name -> M Lir.Ty
lookupTy name = do
  tyMap <- ask
  case tyMap ^. at name of
    Just ty -> pure ty
    Nothing -> throwError $ "could not find variable ".t <> Cfg.nameText name

elabBlock :: Cfg.Block (Lir.Instr Cfg.Name) -> M (Cfg.Block (Lir.Instr Lir.Value))
elabBlock block = Cfg.traverseBlock elabInstr block
  where
    elabInstr :: forall c. Lir.Instr Cfg.Name c -> M (Lir.Instr Lir.Value c)
    elabInstr instr = case instr of
      Lir.Assign name op -> do
        op <- forOf Lir.opInstrUses op \name -> do
          ty <- lookupTy name
          pure $ Lir.Value {Lir.name, Lir.ty}
        let opTy = Lir.getOpInstrTy op
        pure $ Lir.Assign Lir.Value {Lir.name, ty = opTy} op
      Lir.BlockArgs args -> pure $ Lir.BlockArgs args
      Lir.Control instr -> do
        instr <- forOf Lir.controlInstrUses instr \name -> do
          ty <- lookupTy name
          pure $ Lir.Value {Lir.name, Lir.ty}
        pure $ Lir.Control instr
