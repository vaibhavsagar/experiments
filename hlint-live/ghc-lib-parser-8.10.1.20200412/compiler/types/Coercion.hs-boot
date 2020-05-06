{-# LANGUAGE FlexibleContexts #-}

module Coercion where

import GhcPrelude

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} TyCon

import BasicTypes ( LeftOrRight )
import CoAxiom
import Var
import Pair
import Util

mkReflCo :: Role -> Type -> Coercion
mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkAppCo :: Coercion -> Coercion -> Coercion
mkForAllCo :: TyCoVar -> Coercion -> Coercion -> Coercion
mkFunCo :: Role -> Coercion -> Coercion -> Coercion
mkCoVarCo :: CoVar -> Coercion
mkAxiomInstCo :: CoAxiom Branched -> BranchIndex -> [Coercion] -> Coercion
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkUnsafeCo :: Role -> Type -> Type -> Coercion
mkUnivCo :: UnivCoProvenance -> Role -> Type -> Type -> Coercion
mkSymCo :: Coercion -> Coercion
mkTransCo :: Coercion -> Coercion -> Coercion
mkNthCo :: HasDebugCallStack => Role -> Int -> Coercion -> Coercion
mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkInstCo :: Coercion -> Coercion -> Coercion
mkGReflCo :: Role -> Type -> MCoercionN -> Coercion
mkNomReflCo :: Type -> Coercion
mkKindCo :: Coercion -> Coercion
mkSubCo :: Coercion -> Coercion
mkProofIrrelCo :: Role -> Coercion -> Coercion -> Coercion -> Coercion
mkAxiomRuleCo :: CoAxiomRule -> [Coercion] -> Coercion

isGReflCo :: Coercion -> Bool
isReflCo :: Coercion -> Bool
isReflexiveCo :: Coercion -> Bool
decomposePiCos :: HasDebugCallStack => Coercion -> Pair Type -> [Type] -> ([Coercion], Coercion)
coVarKindsTypesRole :: HasDebugCallStack => CoVar -> (Kind, Kind, Type, Type, Role)
coVarRole :: CoVar -> Role

mkCoercionType :: Role -> Type -> Type -> Type

data LiftingContext
liftCoSubst :: HasDebugCallStack => Role -> LiftingContext -> Type -> Coercion
seqCo :: Coercion -> ()

coercionKind :: Coercion -> Pair Type
coercionType :: Coercion -> Type
