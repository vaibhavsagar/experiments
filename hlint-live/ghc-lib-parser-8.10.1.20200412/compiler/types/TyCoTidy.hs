{-# LANGUAGE BangPatterns #-}

-- | Tidying types and coercions for printing in error messages.
module TyCoTidy
  (
        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyVarBndr, tidyVarBndrs, tidyFreeTyCoVars, avoidNameClashes,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyCoVarOcc,
        tidyTopType,
        tidyKind,
        tidyCo, tidyCos,
        tidyTyCoVarBinder, tidyTyCoVarBinders
  ) where

import GhcPrelude

import TyCoRep
import TyCoFVs (tyCoVarsOfTypesWellScoped, tyCoVarsOfTypeList)

import Name hiding (varName)
import Var
import VarEnv
import Util (seqList)

import Data.List (mapAccumL)

{-
%************************************************************************
%*                                                                      *
\subsection{TidyType}
%*                                                                      *
%************************************************************************
-}

-- | This tidies up a type for printing in an error message, or in
-- an interface file.
--
-- It doesn't change the uniques at all, just the print names.
tidyVarBndrs :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyVarBndrs tidy_env tvs
  = mapAccumL tidyVarBndr (avoidNameClashes tvs tidy_env) tvs

tidyVarBndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
tidyVarBndr tidy_env@(occ_env, subst) var
  = case tidyOccName occ_env (getHelpfulOccName var) of
      (occ_env', occ') -> ((occ_env', subst'), var')
        where
          subst' = extendVarEnv subst var var'
          var'   = setVarType (setVarName var name') type'
          type'  = tidyType tidy_env (varType var)
          name'  = tidyNameOcc name occ'
          name   = varName var

avoidNameClashes :: [TyCoVar] -> TidyEnv -> TidyEnv
-- Seed the occ_env with clashes among the names, see
-- Note [Tidying multiple names at once] in OccName
avoidNameClashes tvs (occ_env, subst)
  = (avoidClashesOccEnv occ_env occs, subst)
  where
    occs = map getHelpfulOccName tvs

getHelpfulOccName :: TyCoVar -> OccName
-- A TcTyVar with a System Name is probably a
-- unification variable; when we tidy them we give them a trailing
-- "0" (or 1 etc) so that they don't take precedence for the
-- un-modified name. Plus, indicating a unification variable in
-- this way is a helpful clue for users
getHelpfulOccName tv
  | isSystemName name, isTcTyVar tv
  = mkTyVarOcc (occNameString occ ++ "0")
  | otherwise
  = occ
  where
   name = varName tv
   occ  = getOccName name

tidyTyCoVarBinder :: TidyEnv -> VarBndr TyCoVar vis
                  -> (TidyEnv, VarBndr TyCoVar vis)
tidyTyCoVarBinder tidy_env (Bndr tv vis)
  = (tidy_env', Bndr tv' vis)
  where
    (tidy_env', tv') = tidyVarBndr tidy_env tv

tidyTyCoVarBinders :: TidyEnv -> [VarBndr TyCoVar vis]
                   -> (TidyEnv, [VarBndr TyCoVar vis])
tidyTyCoVarBinders tidy_env tvbs
  = mapAccumL tidyTyCoVarBinder
              (avoidNameClashes (binderVars tvbs) tidy_env) tvbs

---------------
tidyFreeTyCoVars :: TidyEnv -> [TyCoVar] -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyCoVars (full_occ_env, var_env) tyvars
  = fst (tidyOpenTyCoVars (full_occ_env, var_env) tyvars)

---------------
tidyOpenTyCoVars :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyOpenTyCoVars env tyvars = mapAccumL tidyOpenTyCoVar env tyvars

---------------
tidyOpenTyCoVar :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
-- ^ Treat a new 'TyCoVar' as a binder, and give it a fresh tidy name
-- using the environment if one has not already been allocated. See
-- also 'tidyVarBndr'
tidyOpenTyCoVar env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')              -- Already substituted
        Nothing     ->
          let env' = tidyFreeTyCoVars env (tyCoVarsOfTypeList (tyVarKind tyvar))
          in tidyVarBndr env' tyvar  -- Treat it as a binder

---------------
tidyTyCoVarOcc :: TidyEnv -> TyCoVar -> TyCoVar
tidyTyCoVarOcc env@(_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> updateVarType (tidyType env) tv
        Just tv' -> tv'

---------------
tidyTypes :: TidyEnv -> [Type] -> [Type]
tidyTypes env tys = map (tidyType env) tys

---------------
tidyType :: TidyEnv -> Type -> Type
tidyType _   (LitTy n)             = LitTy n
tidyType env (TyVarTy tv)          = TyVarTy (tidyTyCoVarOcc env tv)
tidyType env (TyConApp tycon tys)  = let args = tidyTypes env tys
                                     in args `seqList` TyConApp tycon args
tidyType env (AppTy fun arg)       = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env ty@(FunTy _ arg res)  = let { !arg' = tidyType env arg
                                         ; !res' = tidyType env res }
                                     in ty { ft_arg = arg', ft_res = res' }
tidyType env (ty@(ForAllTy{}))     = mkForAllTys' (zip tvs' vis) $! tidyType env' body_ty
  where
    (tvs, vis, body_ty) = splitForAllTys' ty
    (env', tvs') = tidyVarBndrs env tvs
tidyType env (CastTy ty co)       = (CastTy $! tidyType env ty) $! (tidyCo env co)
tidyType env (CoercionTy co)      = CoercionTy $! (tidyCo env co)


-- The following two functions differ from mkForAllTys and splitForAllTys in that
-- they expect/preserve the ArgFlag argument. Thes belong to types/Type.hs, but
-- how should they be named?
mkForAllTys' :: [(TyCoVar, ArgFlag)] -> Type -> Type
mkForAllTys' tvvs ty = foldr strictMkForAllTy ty tvvs
  where
    strictMkForAllTy (tv,vis) ty = (ForAllTy $! ((Bndr $! tv) $! vis)) $! ty

splitForAllTys' :: Type -> ([TyCoVar], [ArgFlag], Type)
splitForAllTys' ty = go ty [] []
  where
    go (ForAllTy (Bndr tv vis) ty) tvs viss = go ty (tv:tvs) (vis:viss)
    go ty                          tvs viss = (reverse tvs, reverse viss, ty)


---------------
-- | Grabs the free type variables, tidies them
-- and then uses 'tidyType' to work over the type itself
tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys
  = (env', tidyTypes (trimmed_occ_env, var_env) tys)
  where
    (env'@(_, var_env), tvs') = tidyOpenTyCoVars env $
                                tyCoVarsOfTypesWellScoped tys
    trimmed_occ_env = initTidyOccEnv (map getOccName tvs')
      -- The idea here was that we restrict the new TidyEnv to the
      -- _free_ vars of the types, so that we don't gratuitously rename
      -- the _bound_ variables of the types.

---------------
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty = let (env', [ty']) = tidyOpenTypes env [ty] in
                      (env', ty')

---------------
-- | Calls 'tidyType' on a top-level type (i.e. with an empty tidying environment)
tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty

---------------
tidyOpenKind :: TidyEnv -> Kind -> (TidyEnv, Kind)
tidyOpenKind = tidyOpenType

tidyKind :: TidyEnv -> Kind -> Kind
tidyKind = tidyType

----------------
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env@(_, subst) co
  = go co
  where
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo (go co)

    go (Refl ty)             = Refl (tidyType env ty)
    go (GRefl r ty mco)      = GRefl r (tidyType env ty) $! go_mco mco
    go (TyConAppCo r tc cos) = let args = map go cos
                               in args `seqList` TyConAppCo r tc args
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv h co)    = ((ForAllCo $! tvp) $! (go h)) $! (tidyCo envp co)
                               where (envp, tvp) = tidyVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go (FunCo r co1 co2)     = (FunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = case lookupVarEnv subst cv of
                                 Nothing  -> CoVarCo cv
                                 Just cv' -> CoVarCo cv'
    go (HoleCo h)            = HoleCo h
    go (AxiomInstCo con ind cos) = let args = map go cos
                               in  args `seqList` AxiomInstCo con ind args
    go (UnivCo p r t1 t2)    = (((UnivCo $! (go_prov p)) $! r) $!
                                tidyType env t1) $! tidyType env t2
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (NthCo r d co)        = NthCo r d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! go ty
    go (KindCo co)           = KindCo $! go co
    go (SubCo co)            = SubCo $! go co
    go (AxiomRuleCo ax cos)  = let cos1 = tidyCos env cos
                               in cos1 `seqList` AxiomRuleCo ax cos1

    go_prov UnsafeCoerceProv    = UnsafeCoerceProv
    go_prov (PhantomProv co)    = PhantomProv (go co)
    go_prov (ProofIrrelProv co) = ProofIrrelProv (go co)
    go_prov p@(PluginProv _)    = p

tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = map (tidyCo env)


