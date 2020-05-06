{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998
\section[TyCoRep]{Type and Coercion - friends' interface}

Note [The Type-related module hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Class
  CoAxiom
  TyCon    imports Class, CoAxiom
  TyCoRep  imports Class, CoAxiom, TyCon
  TyCoPpr  imports TyCoRep
  TyCoFVs  imports TyCoRep
  TyCoSubst imports TyCoRep, TyCoFVs, TyCoPpr
  TyCoTidy imports TyCoRep, TyCoFVs
  TysPrim  imports TyCoRep ( including mkTyConTy )
  Coercion imports Type
-}

-- We expose the relevant stuff from this module via the Type module
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP, DeriveDataTypeable, MultiWayIf, PatternSynonyms, BangPatterns #-}

module TyCoRep (
        TyThing(..), tyThingCategory, pprTyThingCategory, pprShortTyThing,

        -- * Types
        Type( TyVarTy, AppTy, TyConApp, ForAllTy
            , LitTy, CastTy, CoercionTy
            , FunTy, ft_arg, ft_res, ft_af
            ),  -- Export the type synonym FunTy too

        TyLit(..),
        KindOrType, Kind,
        KnotTied,
        PredType, ThetaType,      -- Synonyms
        ArgFlag(..), AnonArgFlag(..), ForallVisFlag(..),

        -- * Coercions
        Coercion(..),
        UnivCoProvenance(..),
        CoercionHole(..), coHoleCoVar, setCoHoleCoVar,
        CoercionN, CoercionR, CoercionP, KindCoercion,
        MCoercion(..), MCoercionR, MCoercionN,

        -- * Functions over types
        mkTyConTy, mkTyVarTy, mkTyVarTys,
        mkTyCoVarTy, mkTyCoVarTys,
        mkFunTy, mkVisFunTy, mkInvisFunTy, mkVisFunTys, mkInvisFunTys,
        mkForAllTy, mkForAllTys,
        mkPiTy, mkPiTys,

        -- * Functions over binders
        TyCoBinder(..), TyCoVarBinder, TyBinder,
        binderVar, binderVars, binderType, binderArgFlag,
        delBinderVar,
        isInvisibleArgFlag, isVisibleArgFlag,
        isInvisibleBinder, isVisibleBinder,
        isTyBinder, isNamedBinder,

        -- * Functions over coercions
        pickLR,

        -- * Sizes
        typeSize, coercionSize, provSize
    ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} TyCoPpr ( pprType, pprCo, pprTyLit )

   -- Transitively pulls in a LOT of stuff, better to break the loop

import {-# SOURCE #-} ConLike ( ConLike(..), conLikeName )

-- friends:
import IfaceType
import Var
import VarSet
import Name hiding ( varName )
import TyCon
import CoAxiom

-- others
import BasicTypes ( LeftOrRight(..), pickLR )
import Outputable
import FastString
import Util

-- libraries
import qualified Data.Data as Data hiding ( TyCon )
import Data.IORef ( IORef )   -- for CoercionHole

{-
%************************************************************************
%*                                                                      *
                        TyThing
%*                                                                      *
%************************************************************************

Despite the fact that DataCon has to be imported via a hi-boot route,
this module seems the right place for TyThing, because it's needed for
funTyCon and all the types in TysPrim.

It is also SOURCE-imported into Name.hs


Note [ATyCon for classes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Both classes and type constructors are represented in the type environment
as ATyCon.  You can tell the difference, and get to the class, with
   isClassTyCon :: TyCon -> Bool
   tyConClass_maybe :: TyCon -> Maybe Class
The Class and its associated TyCon have the same Name.
-}

-- | A global typecheckable-thing, essentially anything that has a name.
-- Not to be confused with a 'TcTyThing', which is also a typecheckable
-- thing but in the *local* context.  See 'TcEnv' for how to retrieve
-- a 'TyThing' given a 'Name'.
data TyThing
  = AnId     Id
  | AConLike ConLike
  | ATyCon   TyCon       -- TyCons and classes; see Note [ATyCon for classes]
  | ACoAxiom (CoAxiom Branched)

instance Outputable TyThing where
  ppr = pprShortTyThing

instance NamedThing TyThing where       -- Can't put this with the type
  getName (AnId id)     = getName id    -- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc    -- isn't visible there
  getName (ACoAxiom cc) = getName cc
  getName (AConLike cl) = conLikeName cl

pprShortTyThing :: TyThing -> SDoc
-- c.f. PprTyThing.pprTyThing, which prints all the details
pprShortTyThing thing
  = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory = text . capitalise . tyThingCategory

tyThingCategory :: TyThing -> String
tyThingCategory (ATyCon tc)
  | isClassTyCon tc = "class"
  | otherwise       = "type constructor"
tyThingCategory (ACoAxiom _) = "coercion axiom"
tyThingCategory (AnId   _)   = "identifier"
tyThingCategory (AConLike (RealDataCon _)) = "data constructor"
tyThingCategory (AConLike (PatSynCon _))  = "pattern synonym"


{- **********************************************************************
*                                                                       *
                        Type
*                                                                       *
********************************************************************** -}

-- | The key representation of types within the compiler

type KindOrType = Type -- See Note [Arguments to type constructors]

-- | The key type representing kinds in the compiler.
type Kind = Type

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Type
  -- See Note [Non-trivial definitional equality]
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --     See Note [Respecting definitional equality] (EQ1) about the
                        --     no 'CastTy' requirement
                        --
                        --  2) Argument type

  | TyConApp
        TyCon
        [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | ForAllTy
        {-# UNPACK #-} !TyCoVarBinder
        Type            -- ^ A Π type.

  | FunTy      -- ^ t1 -> t2   Very common, so an important special case
                -- See Note [Function types]
     { ft_af  :: AnonArgFlag  -- Is this (->) or (=>)?
     , ft_arg :: Type           -- Argument type
     , ft_res :: Type }         -- Result type

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never refl.
                      -- INVARIANT: The Type is not a CastTy (use TransCo instead)
                      -- See Note [Respecting definitional equality] (EQ2) and (EQ3)

  | CoercionTy
        Coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor

  deriving Data.Data

instance Outputable Type where
  ppr = pprType

-- NOTE:  Other parts of the code assume that type literals do not contain
-- types or type variables.
data TyLit
  = NumTyLit Integer
  | StrTyLit FastString
  deriving (Eq, Ord, Data.Data)

instance Outputable TyLit where
   ppr = pprTyLit

{- Note [Function types]
~~~~~~~~~~~~~~~~~~~~~~~~
FFunTy is the constructor for a function type.  Lots of things to say
about it!

* FFunTy is the data constructor, meaning "full function type".

* The function type constructor (->) has kind
     (->) :: forall r1 r2. TYPE r1 -> TYPE r2 -> Type LiftedRep
  mkTyConApp ensure that we convert a saturated application
    TyConApp (->) [r1,r2,t1,t2] into FunTy t1 t2
  dropping the 'r1' and 'r2' arguments; they are easily recovered
  from 't1' and 't2'.

* The ft_af field says whether or not this is an invisible argument
     VisArg:   t1 -> t2    Ordinary function type
     InvisArg: t1 => t2    t1 is guaranteed to be a predicate type,
                           i.e. t1 :: Constraint
  See Note [Types for coercions, predicates, and evidence]

  This visibility info makes no difference in Core; it matters
  only when we regard the type as a Haskell source type.

* FunTy is a (unidirectional) pattern synonym that allows
  positional pattern matching (FunTy arg res), ignoring the
  ArgFlag.
-}

{- -----------------------
      Commented out until the pattern match
      checker can handle it; see #16185

      For now we use the CPP macro #define FunTy FFunTy _
      (see HsVersions.h) to allow pattern matching on a
      (positional) FunTy constructor.

{-# COMPLETE FunTy, TyVarTy, AppTy, TyConApp
           , ForAllTy, LitTy, CastTy, CoercionTy :: Type #-}

-- | 'FunTy' is a (uni-directional) pattern synonym for the common
-- case where we want to match on the argument/result type, but
-- ignoring the AnonArgFlag
pattern FunTy :: Type -> Type -> Type
pattern FunTy arg res <- FFunTy { ft_arg = arg, ft_res = res }

       End of commented out block
---------------------------------- -}

{- Note [Types for coercions, predicates, and evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat differently:

  (a) Predicate types
        Test: isPredTy
        Binders: DictIds
        Kind: Constraint
        Examples: (Eq a), and (a ~ b)

  (b) Coercion types are primitive, unboxed equalities
        Test: isCoVarTy
        Binders: CoVars (can appear in coercions)
        Kind: TYPE (TupleRep [])
        Examples: (t1 ~# t2) or (t1 ~R# t2)

  (c) Evidence types is the type of evidence manipulated by
      the type constraint solver.
        Test: isEvVarType
        Binders: EvVars
        Kind: Constraint or TYPE (TupleRep [])
        Examples: all coercion types and predicate types

Coercion types and predicate types are mutually exclusive,
but evidence types are a superset of both.

When treated as a user type,

  - Predicates (of kind Constraint) are invisible and are
    implicitly instantiated

  - Coercion types, and non-pred evidence types (i.e. not
    of kind Constrain), are just regular old types, are
    visible, and are not implicitly instantiated.

In a FunTy { ft_af = InvisArg }, the argument type is always
a Predicate type.

Note [Constraints in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do we allow a type constructor to have a kind like
   S :: Eq a => a -> Type

No, we do not.  Doing so would mean would need a TyConApp like
   S @k @(d :: Eq k) (ty :: k)
 and we have no way to build, or decompose, evidence like
 (d :: Eq k) at the type level.

But we admit one exception: equality.  We /do/ allow, say,
   MkT :: (a ~ b) => a -> b -> Type a b

Why?  Because we can, without much difficulty.  Moreover
we can promote a GADT data constructor (see TyCon
Note [Promoted data constructors]), like
  data GT a b where
    MkGT : a -> a -> GT a a
so programmers might reasonably expect to be able to
promote MkT as well.

How does this work?

* In TcValidity.checkConstraintsOK we reject kinds that
  have constraints other than (a~b) and (a~~b).

* In Inst.tcInstInvisibleTyBinder we instantiate a call
  of MkT by emitting
     [W] co :: alpha ~# beta
  and producing the elaborated term
     MkT @alpha @beta (Eq# alpha beta co)
  We don't generate a boxed "Wanted"; we generate only a
  regular old /unboxed/ primitive-equality Wanted, and build
  the box on the spot.

* How can we get such a MkT?  By promoting a GADT-style data
  constructor
     data T a b where
       MkT :: (a~b) => a -> b -> T a b
  See DataCon.mkPromotedDataCon
  and Note [Promoted data constructors] in TyCon

* We support both homogeneous (~) and heterogeneous (~~)
  equality.  (See Note [The equality types story]
  in TysPrim for a primer on these equality types.)

* How do we prevent a MkT having an illegal constraint like
  Eq a?  We check for this at use-sites; see TcHsType.tcTyVar,
  specifically dc_theta_illegal_constraint.

* Notice that nothing special happens if
    K :: (a ~# b) => blah
  because (a ~# b) is not a predicate type, and is never
  implicitly instantiated. (Mind you, it's not clear how you
  could creates a type constructor with such a kind.) See
  Note [Types for coercions, predicates, and evidence]

* The existence of promoted MkT with an equality-constraint
  argument is the (only) reason that the AnonTCB constructor
  of TyConBndrVis carries an AnonArgFlag (VisArg/InvisArg).
  For example, when we promote the data constructor
     MkT :: forall a b. (a~b) => a -> b -> T a b
  we get a PromotedDataCon with tyConBinders
      Bndr (a :: Type)  (NamedTCB Inferred)
      Bndr (b :: Type)  (NamedTCB Inferred)
      Bndr (_ :: a ~ b) (AnonTCB InvisArg)
      Bndr (_ :: a)     (AnonTCB VisArg))
      Bndr (_ :: b)     (AnonTCB VisArg))

* One might reasonably wonder who *unpacks* these boxes once they are
  made. After all, there is no type-level `case` construct. The
  surprising answer is that no one ever does. Instead, if a GADT
  constructor is used on the left-hand side of a type family equation,
  that occurrence forces GHC to unify the types in question. For
  example:

  data G a where
    MkG :: G Bool

  type family F (x :: G a) :: a where
    F MkG = False

  When checking the LHS `F MkG`, GHC sees the MkG constructor and then must
  unify F's implicit parameter `a` with Bool. This succeeds, making the equation

    F Bool (MkG @Bool <Bool>) = False

  Note that we never need unpack the coercion. This is because type
  family equations are *not* parametric in their kind variables. That
  is, we could have just said

  type family H (x :: G a) :: a where
    H _ = False

  The presence of False on the RHS also forces `a` to become Bool,
  giving us

    H Bool _ = False

  The fact that any of this works stems from the lack of phase
  separation between types and kinds (unlike the very present phase
  separation between terms and types).

  Once we have the ability to pattern-match on types below top-level,
  this will no longer cut it, but it seems fine for now.


Note [Arguments to type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of kind polymorphism, in addition to type application we now
have kind instantiation. We reuse the same notations to do so.

For example:

  Just (* -> *) Maybe
  Right * Nat Zero

are represented by:

  TyConApp (PromotedDataCon Just) [* -> *, Maybe]
  TyConApp (PromotedDataCon Right) [*, Nat, (PromotedDataCon Zero)]

Important note: Nat is used as a *kind* and not as a type. This can be
confusing, since type-level Nat and kind-level Nat are identical. We
use the kind of (PromotedDataCon Right) to know if its arguments are
kinds or types.

This kind instantiation only happens in TyConApp currently.

Note [Non-trivial definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is Int |> <*> the same as Int? YES! In order to reduce headaches,
we decide that any reflexive casts in types are just ignored.
(Indeed they must be. See Note [Respecting definitional equality].)
More generally, the `eqType` function, which defines Core's type equality
relation, ignores casts and coercion arguments, as long as the
two types have the same kind. This allows us to be a little sloppier
in keeping track of coercions, which is a good thing. It also means
that eqType does not depend on eqCoercion, which is also a good thing.

Why is this sensible? That is, why is something different than α-equivalence
appropriate for the implementation of eqType?

Anything smaller than ~ and homogeneous is an appropriate definition for
equality. The type safety of FC depends only on ~. Let's say η : τ ~ σ. Any
expression of type τ can be transmuted to one of type σ at any point by
casting. The same is true of expressions of type σ. So in some sense, τ and σ
are interchangeable.

But let's be more precise. If we examine the typing rules of FC (say, those in
https://cs.brynmawr.edu/~rae/papers/2015/equalities/equalities.pdf)
there are several places where the same metavariable is used in two different
premises to a rule. (For example, see Ty_App.) There is an implicit equality
check here. What definition of equality should we use? By convention, we use
α-equivalence. Take any rule with one (or more) of these implicit equality
checks. Then there is an admissible rule that uses ~ instead of the implicit
check, adding in casts as appropriate.

The only problem here is that ~ is heterogeneous. To make the kinds work out
in the admissible rule that uses ~, it is necessary to homogenize the
coercions. That is, if we have η : (τ : κ1) ~ (σ : κ2), then we don't use η;
we use η |> kind η, which is homogeneous.

The effect of this all is that eqType, the implementation of the implicit
equality check, can use any homogeneous relation that is smaller than ~, as
those rules must also be admissible.

A more drawn out argument around all of this is presented in Section 7.2 of
Richard E's thesis (http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf).

What would go wrong if we insisted on the casts matching? See the beginning of
Section 8 in the unpublished paper above. Theoretically, nothing at all goes
wrong. But in practical terms, getting the coercions right proved to be
nightmarish. And types would explode: during kind-checking, we often produce
reflexive kind coercions. When we try to cast by these, mkCastTy just discards
them. But if we used an eqType that distinguished between Int and Int |> <*>,
then we couldn't discard -- the output of kind-checking would be enormous,
and we would need enormous casts with lots of CoherenceCo's to straighten
them out.

Would anything go wrong if eqType respected type families? No, not at all. But
that makes eqType rather hard to implement.

Thus, the guideline for eqType is that it should be the largest
easy-to-implement relation that is still smaller than ~ and homogeneous. The
precise choice of relation is somewhat incidental, as long as the smart
constructors and destructors in Type respect whatever relation is chosen.

Another helpful principle with eqType is this:

 (EQ) If (t1 `eqType` t2) then I can replace t1 by t2 anywhere.

This principle also tells us that eqType must relate only types with the
same kinds.

Note [Respecting definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Non-trivial definitional equality] introduces the property (EQ).
How is this upheld?

Any function that pattern matches on all the constructors will have to
consider the possibility of CastTy. Presumably, those functions will handle
CastTy appropriately and we'll be OK.

More dangerous are the splitXXX functions. Let's focus on splitTyConApp.
We don't want it to fail on (T a b c |> co). Happily, if we have
  (T a b c |> co) `eqType` (T d e f)
then co must be reflexive. Why? eqType checks that the kinds are equal, as
well as checking that (a `eqType` d), (b `eqType` e), and (c `eqType` f).
By the kind check, we know that (T a b c |> co) and (T d e f) have the same
kind. So the only way that co could be non-reflexive is for (T a b c) to have
a different kind than (T d e f). But because T's kind is closed (all tycon kinds
are closed), the only way for this to happen is that one of the arguments has
to differ, leading to a contradiction. Thus, co is reflexive.

Accordingly, by eliminating reflexive casts, splitTyConApp need not worry
about outermost casts to uphold (EQ). Eliminating reflexive casts is done
in mkCastTy.

Unforunately, that's not the end of the story. Consider comparing
  (T a b c)      =?       (T a b |> (co -> <Type>)) (c |> co)
These two types have the same kind (Type), but the left type is a TyConApp
while the right type is not. To handle this case, we say that the right-hand
type is ill-formed, requiring an AppTy never to have a casted TyConApp
on its left. It is easy enough to pull around the coercions to maintain
this invariant, as done in Type.mkAppTy. In the example above, trying to
form the right-hand type will instead yield (T a b (c |> co |> sym co) |> <Type>).
Both the casts there are reflexive and will be dropped. Huzzah.

This idea of pulling coercions to the right works for splitAppTy as well.

However, there is one hiccup: it's possible that a coercion doesn't relate two
Pi-types. For example, if we have @type family Fun a b where Fun a b = a -> b@,
then we might have (T :: Fun Type Type) and (T |> axFun) Int. That axFun can't
be pulled to the right. But we don't need to pull it: (T |> axFun) Int is not
`eqType` to any proper TyConApp -- thus, leaving it where it is doesn't violate
our (EQ) property.

Lastly, in order to detect reflexive casts reliably, we must make sure not
to have nested casts: we update (t |> co1 |> co2) to (t |> (co1 `TransCo` co2)).

In sum, in order to uphold (EQ), we need the following three invariants:

  (EQ1) No decomposable CastTy to the left of an AppTy, where a decomposable
        cast is one that relates either a FunTy to a FunTy or a
        ForAllTy to a ForAllTy.
  (EQ2) No reflexive casts in CastTy.
  (EQ3) No nested CastTys.
  (EQ4) No CastTy over (ForAllTy (Bndr tyvar vis) body).
        See Note [Weird typing rule for ForAllTy] in Type.

These invariants are all documented above, in the declaration for Type.

Note [Unused coercion variable in ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  \(co:t1 ~ t2). e

What type should we give to this expression?
  (1) forall (co:t1 ~ t2) -> t
  (2) (t1 ~ t2) -> t

If co is used in t, (1) should be the right choice.
if co is not used in t, we would like to have (1) and (2) equivalent.

However, we want to keep eqType simple and don't want eqType (1) (2) to return
True in any case.

We decide to always construct (2) if co is not used in t.

Thus in mkLamType, we check whether the variable is a coercion
variable (of type (t1 ~# t2), and whether it is un-used in the
body. If so, it returns a FunTy instead of a ForAllTy.

There are cases we want to skip the check. For example, the check is
unnecessary when it is known from the context that the input variable
is a type variable.  In those cases, we use mkForAllTy.

-}

-- | A type labeled 'KnotTied' might have knot-tied tycons in it. See
-- Note [Type checking recursive type and class declarations] in
-- TcTyClsDecls
type KnotTied ty = ty

{- **********************************************************************
*                                                                       *
                  TyCoBinder and ArgFlag
*                                                                       *
********************************************************************** -}

-- | A 'TyCoBinder' represents an argument to a function. TyCoBinders can be
-- dependent ('Named') or nondependent ('Anon'). They may also be visible or
-- not. See Note [TyCoBinders]
data TyCoBinder
  = Named TyCoVarBinder    -- A type-lambda binder
  | Anon AnonArgFlag Type  -- A term-lambda binder. Type here can be CoercionTy.
                           -- Visibility is determined by the AnonArgFlag
  deriving Data.Data

instance Outputable TyCoBinder where
  ppr (Anon af ty) = ppr af <+> ppr ty
  ppr (Named (Bndr v Required))  = ppr v
  ppr (Named (Bndr v Specified)) = char '@' <> ppr v
  ppr (Named (Bndr v Inferred))  = braces (ppr v)


-- | 'TyBinder' is like 'TyCoBinder', but there can only be 'TyVarBinder'
-- in the 'Named' field.
type TyBinder = TyCoBinder

-- | Remove the binder's variable from the set, if the binder has
-- a variable.
delBinderVar :: VarSet -> TyCoVarBinder -> VarSet
delBinderVar vars (Bndr tv _) = vars `delVarSet` tv

-- | Does this binder bind an invisible argument?
isInvisibleBinder :: TyCoBinder -> Bool
isInvisibleBinder (Named (Bndr _ vis)) = isInvisibleArgFlag vis
isInvisibleBinder (Anon InvisArg _)    = True
isInvisibleBinder (Anon VisArg   _)    = False

-- | Does this binder bind a visible argument?
isVisibleBinder :: TyCoBinder -> Bool
isVisibleBinder = not . isInvisibleBinder

isNamedBinder :: TyCoBinder -> Bool
isNamedBinder (Named {}) = True
isNamedBinder (Anon {})  = False

-- | If its a named binder, is the binder a tyvar?
-- Returns True for nondependent binder.
-- This check that we're really returning a *Ty*Binder (as opposed to a
-- coercion binder). That way, if/when we allow coercion quantification
-- in more places, we'll know we missed updating some function.
isTyBinder :: TyCoBinder -> Bool
isTyBinder (Named bnd) = isTyVarBinder bnd
isTyBinder _ = True

{- Note [TyCoBinders]
~~~~~~~~~~~~~~~~~~~
A ForAllTy contains a TyCoVarBinder.  But a type can be decomposed
to a telescope consisting of a [TyCoBinder]

A TyCoBinder represents the type of binders -- that is, the type of an
argument to a Pi-type. GHC Core currently supports two different
Pi-types:

 * A non-dependent function type,
   written with ->, e.g. ty1 -> ty2
   represented as FunTy ty1 ty2. These are
   lifted to Coercions with the corresponding FunCo.

 * A dependent compile-time-only polytype,
   written with forall, e.g.  forall (a:*). ty
   represented as ForAllTy (Bndr a v) ty

Both Pi-types classify terms/types that take an argument. In other
words, if `x` is either a function or a polytype, `x arg` makes sense
(for an appropriate `arg`).


Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A ForAllTy (used for both types and kinds) contains a TyCoVarBinder.
  Each TyCoVarBinder
      Bndr a tvis
  is equipped with tvis::ArgFlag, which says whether or not arguments
  for this binder should be visible (explicit) in source Haskell.

* A TyCon contains a list of TyConBinders.  Each TyConBinder
      Bndr a cvis
  is equipped with cvis::TyConBndrVis, which says whether or not type
  and kind arguments for this TyCon should be visible (explicit) in
  source Haskell.

This table summarises the visibility rules:
---------------------------------------------------------------------------------------
|                                                      Occurrences look like this
|                             GHC displays type as     in Haskell source code
|--------------------------------------------------------------------------------------
| Bndr a tvis :: TyCoVarBinder, in the binder of ForAllTy for a term
|  tvis :: ArgFlag
|  tvis = Inferred:            f :: forall {a}. type    Arg not allowed:  f
                               f :: forall {co}. type   Arg not allowed:  f
|  tvis = Specified:           f :: forall a. type      Arg optional:     f  or  f @Int
|  tvis = Required:            T :: forall k -> type    Arg required:     T *
|    This last form is illegal in terms: See Note [No Required TyCoBinder in terms]
|
| Bndr k cvis :: TyConBinder, in the TyConBinders of a TyCon
|  cvis :: TyConBndrVis
|  cvis = AnonTCB:             T :: kind -> kind        Required:            T *
|  cvis = NamedTCB Inferred:   T :: forall {k}. kind    Arg not allowed:     T
|                              T :: forall {co}. kind   Arg not allowed:     T
|  cvis = NamedTCB Specified:  T :: forall k. kind      Arg not allowed[1]:  T
|  cvis = NamedTCB Required:   T :: forall k -> kind    Required:            T *
---------------------------------------------------------------------------------------

[1] In types, in the Specified case, it would make sense to allow
    optional kind applications, thus (T @*), but we have not
    yet implemented that

---- In term declarations ----

* Inferred.  Function defn, with no signature:  f1 x = x
  We infer f1 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because it doesn't appear in any
  user-written signature for f1

* Specified.  Function defn, with signature (implicit forall):
     f2 :: a -> a; f2 x = x
  So f2 gets the type f2 :: forall a. a -> a, with 'a' Specified
  even though 'a' is not bound in the source code by an explicit forall

* Specified.  Function defn, with signature (explicit forall):
     f3 :: forall a. a -> a; f3 x = x
  So f3 gets the type f3 :: forall a. a -> a, with 'a' Specified

* Inferred/Specified.  Function signature with inferred kind polymorphism.
     f4 :: a b -> Int
  So 'f4' gets the type f4 :: forall {k} (a:k->*) (b:k). a b -> Int
  Here 'k' is Inferred (it's not mentioned in the type),
  but 'a' and 'b' are Specified.

* Specified.  Function signature with explicit kind polymorphism
     f5 :: a (b :: k) -> Int
  This time 'k' is Specified, because it is mentioned explicitly,
  so we get f5 :: forall (k:*) (a:k->*) (b:k). a b -> Int

* Similarly pattern synonyms:
  Inferred - from inferred types (e.g. no pattern type signature)
           - or from inferred kind polymorphism

---- In type declarations ----

* Inferred (k)
     data T1 a b = MkT1 (a b)
  Here T1's kind is  T1 :: forall {k:*}. (k->*) -> k -> *
  The kind variable 'k' is Inferred, since it is not mentioned

  Note that 'a' and 'b' correspond to /Anon/ TyCoBinders in T1's kind,
  and Anon binders don't have a visibility flag. (Or you could think
  of Anon having an implicit Required flag.)

* Specified (k)
     data T2 (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall (k:*). (k->*) -> k -> *
  The kind variable 'k' is Specified, since it is mentioned in
  the signature.

* Required (k)
     data T k (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall k:* -> (k->*) -> k -> *
  The kind is Required, since it bound in a positional way in T's declaration
  Every use of T must be explicitly applied to a kind

* Inferred (k1), Specified (k)
     data T a b (c :: k) = MkT (a b) (Proxy c)
  Here T's kind is  T :: forall {k1:*} (k:*). (k1->*) -> k1 -> k -> *
  So 'k' is Specified, because it appears explicitly,
  but 'k1' is Inferred, because it does not

Generally, in the list of TyConBinders for a TyCon,

* Inferred arguments always come first
* Specified, Anon and Required can be mixed

e.g.
  data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where ...

Here Foo's TyConBinders are
   [Required 'a', Specified 'b', Anon]
and its kind prints as
   Foo :: forall a -> forall b. (a -> b -> Type) -> Type

See also Note [Required, Specified, and Inferred for types] in TcTyClsDecls

---- Printing -----

 We print forall types with enough syntax to tell you their visibility
 flag.  But this is not source Haskell, and these types may not all
 be parsable.

 Specified: a list of Specified binders is written between `forall` and `.`:
               const :: forall a b. a -> b -> a

 Inferred:  with -fprint-explicit-foralls, Inferred binders are written
            in braces:
               f :: forall {k} (a:k). S k a -> Int
            Otherwise, they are printed like Specified binders.

 Required: binders are put between `forall` and `->`:
              T :: forall k -> *

---- Other points -----

* In classic Haskell, all named binders (that is, the type variables in
  a polymorphic function type f :: forall a. a -> a) have been Inferred.

* Inferred variables correspond to "generalized" variables from the
  Visible Type Applications paper (ESOP'16).

Note [No Required TyCoBinder in terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Required foralls for term variables, including pattern
synonyms and data constructors.  Why?  Because then an application
would need a /compulsory/ type argument (possibly without an "@"?),
thus (f Int); and we don't have concrete syntax for that.

We could change this decision, but Required, Named TyCoBinders are rare
anyway.  (Most are Anons.)

However the type of a term can (just about) have a required quantifier;
see Note [Required quantifiers in the type of a term] in TcExpr.
-}


{- **********************************************************************
*                                                                       *
                        PredType
*                                                                       *
********************************************************************** -}


-- | A type of the form @p@ of constraint kind represents a value whose type is
-- the Haskell predicate @p@, where a predicate is what occurs before
-- the @=>@ in a Haskell type.
--
-- We use 'PredType' as documentation to mark those types that we guarantee to
-- have this kind.
--
-- It can be expanded into its representation, but:
--
-- * The type checker must treat it as opaque
--
-- * The rest of the compiler treats it as transparent
--
-- Consider these examples:
--
-- > f :: (Eq a) => a -> Int
-- > g :: (?x :: Int -> Int) => a -> Int
-- > h :: (r\l) => {r} => {l::Int | r}
--
-- Here the @Eq a@ and @?x :: Int -> Int@ and @r\l@ are all called \"predicates\"
type PredType = Type

-- | A collection of 'PredType's
type ThetaType = [PredType]

{-
(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using
        * a FunTy for the double arrow
        * with a type of kind Constraint as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (p :: Constraint) then the predicate p is
represented by evidence of type p.


%************************************************************************
%*                                                                      *
            Simple constructors
%*                                                                      *
%************************************************************************

These functions are here so that they can be used by TysPrim,
which in turn is imported by Type
-}

mkTyVarTy  :: TyVar   -> Type
mkTyVarTy v = ASSERT2( isTyVar v, ppr v <+> dcolon <+> ppr (tyVarKind v) )
              TyVarTy v

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

mkTyCoVarTy :: TyCoVar -> Type
mkTyCoVarTy v
  | isTyVar v
  = TyVarTy v
  | otherwise
  = CoercionTy (CoVarCo v)

mkTyCoVarTys :: [TyCoVar] -> [Type]
mkTyCoVarTys = map mkTyCoVarTy

infixr 3 `mkFunTy`, `mkVisFunTy`, `mkInvisFunTy`      -- Associates to the right

mkFunTy :: AnonArgFlag -> Type -> Type -> Type
mkFunTy af arg res = FunTy { ft_af = af, ft_arg = arg, ft_res = res }

mkVisFunTy, mkInvisFunTy :: Type -> Type -> Type
mkVisFunTy   = mkFunTy VisArg
mkInvisFunTy = mkFunTy InvisArg

-- | Make nested arrow types
mkVisFunTys, mkInvisFunTys :: [Type] -> Type -> Type
mkVisFunTys   tys ty = foldr mkVisFunTy   ty tys
mkInvisFunTys tys ty = foldr mkInvisFunTy ty tys

-- | Like 'mkTyCoForAllTy', but does not check the occurrence of the binder
-- See Note [Unused coercion variable in ForAllTy]
mkForAllTy :: TyCoVar -> ArgFlag -> Type -> Type
mkForAllTy tv vis ty = ForAllTy (Bndr tv vis) ty

-- | Wraps foralls over the type using the provided 'TyCoVar's from left to right
mkForAllTys :: [TyCoVarBinder] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

mkPiTy:: TyCoBinder -> Type -> Type
mkPiTy (Anon af ty1) ty2        = FunTy { ft_af = af, ft_arg = ty1, ft_res = ty2 }
mkPiTy (Named (Bndr tv vis)) ty = mkForAllTy tv vis ty

mkPiTys :: [TyCoBinder] -> Type -> Type
mkPiTys tbs ty = foldr mkPiTy ty tbs

-- | Create the plain type constructor type which has been applied to no type arguments at all.
mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []

{-
%************************************************************************
%*                                                                      *
            Coercions
%*                                                                      *
%************************************************************************
-}

-- | A 'Coercion' is concrete evidence of the equality/convertibility
-- of two types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Coercion
  -- Each constructor has a "role signature", indicating the way roles are
  -- propagated through coercions.
  --    -  P, N, and R stand for coercions of the given role
  --    -  e stands for a coercion of a specific unknown role
  --           (think "role polymorphism")
  --    -  "e" stands for an explicit role parameter indicating role e.
  --    -   _ stands for a parameter that is not a Role or Coercion.

  -- These ones mirror the shape of types
  = -- Refl :: _ -> N
    Refl Type  -- See Note [Refl invariant]
          -- Invariant: applications of (Refl T) to a bunch of identity coercions
          --            always show up as Refl.
          -- For example  (Refl T) (Refl a) (Refl b) shows up as (Refl (T a b)).

          -- Applications of (Refl T) to some coercions, at least one of
          -- which is NOT the identity, show up as TyConAppCo.
          -- (They may not be fully saturated however.)
          -- ConAppCo coercions (like all coercions other than Refl)
          -- are NEVER the identity.

          -- Use (GRefl Representational ty MRefl), not (SubCo (Refl ty))

  -- GRefl :: "e" -> _ -> Maybe N -> e
  -- See Note [Generalized reflexive coercion]
  | GRefl Role Type MCoercionN  -- See Note [Refl invariant]
          -- Use (Refl ty), not (GRefl Nominal ty MRefl)
          -- Use (GRefl Representational _ _), not (SubCo (GRefl Nominal _ _))

  -- These ones simply lift the correspondingly-named
  -- Type constructors into Coercions

  -- TyConAppCo :: "e" -> _ -> ?? -> e
  -- See Note [TyConAppCo roles]
  | TyConAppCo Role TyCon [Coercion]    -- lift TyConApp
               -- The TyCon is never a synonym;
               -- we expand synonyms eagerly
               -- But it can be a type function

  | AppCo Coercion CoercionN             -- lift AppTy
          -- AppCo :: e -> N -> e

  -- See Note [Forall coercions]
  | ForAllCo TyCoVar KindCoercion Coercion
         -- ForAllCo :: _ -> N -> e -> e

  | FunCo Role Coercion Coercion         -- lift FunTy
         -- FunCo :: "e" -> e -> e -> e
         -- Note: why doesn't FunCo have a AnonArgFlag, like FunTy?
         -- Because the AnonArgFlag has no impact on Core; it is only
         -- there to guide implicit instantiation of Haskell source
         -- types, and that is irrelevant for coercions, which are
         -- Core-only.

  -- These are special
  | CoVarCo CoVar      -- :: _ -> (N or R)
                       -- result role depends on the tycon of the variable's type

    -- AxiomInstCo :: e -> _ -> ?? -> e
  | AxiomInstCo (CoAxiom Branched) BranchIndex [Coercion]
     -- See also [CoAxiom index]
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom. If there are
     -- any left over, we use AppCo.
     -- See [Coercion axioms applied to coercions]
     -- The roles of the argument coercions are determined
     -- by the cab_roles field of the relevant branch of the CoAxiom

  | AxiomRuleCo CoAxiomRule [Coercion]
    -- AxiomRuleCo is very like AxiomInstCo, but for a CoAxiomRule
    -- The number coercions should match exactly the expectations
    -- of the CoAxiomRule (i.e., the rule is fully saturated).

  | UnivCo UnivCoProvenance Role Type Type
      -- :: _ -> "e" -> _ -> _ -> e

  | SymCo Coercion             -- :: e -> e
  | TransCo Coercion Coercion  -- :: e -> e -> e

  | NthCo  Role Int Coercion     -- Zero-indexed; decomposes (T t0 ... tn)
    -- :: "e" -> _ -> e0 -> e (inverse of TyConAppCo, see Note [TyConAppCo roles])
    -- Using NthCo on a ForAllCo gives an N coercion always
    -- See Note [NthCo and newtypes]
    --
    -- Invariant:  (NthCo r i co), it is always the case that r = role of (Nth i co)
    -- That is: the role of the entire coercion is redundantly cached here.
    -- See Note [NthCo Cached Roles]

  | LRCo   LeftOrRight CoercionN     -- Decomposes (t_left t_right)
    -- :: _ -> N -> N
  | InstCo Coercion CoercionN
    -- :: e -> N -> e
    -- See Note [InstCo roles]

  -- Extract a kind coercion from a (heterogeneous) type coercion
  -- NB: all kind coercions are Nominal
  | KindCo Coercion
     -- :: e -> N

  | SubCo CoercionN                  -- Turns a ~N into a ~R
    -- :: N -> R

  | HoleCo CoercionHole              -- ^ See Note [Coercion holes]
                                     -- Only present during typechecking
  deriving Data.Data

type CoercionN = Coercion       -- always nominal
type CoercionR = Coercion       -- always representational
type CoercionP = Coercion       -- always phantom
type KindCoercion = CoercionN   -- always nominal

instance Outputable Coercion where
  ppr = pprCo

-- | A semantically more meaningful type to represent what may or may not be a
-- useful 'Coercion'.
data MCoercion
  = MRefl
    -- A trivial Reflexivity coercion
  | MCo Coercion
    -- Other coercions
  deriving Data.Data
type MCoercionR = MCoercion
type MCoercionN = MCoercion

instance Outputable MCoercion where
  ppr MRefl    = text "MRefl"
  ppr (MCo co) = text "MCo" <+> ppr co

{-
Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Invariant 1:

Coercions have the following invariant
     Refl (similar for GRefl r ty MRefl) is always lifted as far as possible.

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.

Note [Generalized reflexive coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GRefl is a generalized reflexive coercion (see #15192). It wraps a kind
coercion, which might be reflexive (MRefl) or any coercion (MCo co). The typing
rules for GRefl:

  ty : k1
  ------------------------------------
  GRefl r ty MRefl: ty ~r ty

  ty : k1       co :: k1 ~ k2
  ------------------------------------
  GRefl r ty (MCo co) : ty ~r ty |> co

Consider we have

   g1 :: s ~r t
   s  :: k1
   g2 :: k1 ~ k2

and we want to construct a coercions co which has type

   (s |> g2) ~r t

We can define

   co = Sym (GRefl r s g2) ; g1

It is easy to see that

   Refl == GRefl Nominal ty MRefl :: ty ~n ty

A nominal reflexive coercion is quite common, so we keep the special form Refl to
save allocation.

Note [Coercion axioms applied to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason coercion axioms can be applied to coercions and not just
types is to allow for better optimization.  There are some cases where
we need to be able to "push transitivity inside" an axiom in order to
expose further opportunities for optimization.

For example, suppose we have

  C a : t[a] ~ F a
  g   : b ~ c

and we want to optimize

  sym (C b) ; t[g] ; C c

which has the kind

  F b ~ F c

(stopping through t[b] and t[c] along the way).

We'd like to optimize this to just F g -- but how?  The key is
that we need to allow axioms to be instantiated by *coercions*,
not just by types.  Then we can (in certain cases) push
transitivity inside the axiom instantiations, and then react
opposite-polarity instantiations of the same axiom.  In this
case, e.g., we match t[g] against the LHS of (C c)'s kind, to
obtain the substitution  a |-> g  (note this operation is sort
of the dual of lifting!) and hence end up with

  C g : t[b] ~ F c

which indeed has the same kind as  t[g] ; C c.

Now we have

  sym (C b) ; C g

which can be optimized to F g.

Note [CoAxiom index]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)

The Int in the AxiomInstCo constructor is the 0-indexed number
of the chosen branch.

Note [Forall coercions]
~~~~~~~~~~~~~~~~~~~~~~~
Constructing coercions between forall-types can be a bit tricky,
because the kinds of the bound tyvars can be different.

The typing rule is:


  kind_co : k1 ~ k2
  tv1:k1 |- co : t1 ~ t2
  -------------------------------------------------------------------
  ForAllCo tv1 kind_co co : all tv1:k1. t1  ~
                            all tv1:k2. (t2[tv1 |-> tv1 |> sym kind_co])

First, the TyCoVar stored in a ForAllCo is really an optimisation: this field
should be a Name, as its kind is redundant. Thinking of the field as a Name
is helpful in understanding what a ForAllCo means.
The kind of TyCoVar always matches the left-hand kind of the coercion.

The idea is that kind_co gives the two kinds of the tyvar. See how, in the
conclusion, tv1 is assigned kind k1 on the left but kind k2 on the right.

Of course, a type variable can't have different kinds at the same time. So,
we arbitrarily prefer the first kind when using tv1 in the inner coercion
co, which shows that t1 equals t2.

The last wrinkle is that we need to fix the kinds in the conclusion. In
t2, tv1 is assumed to have kind k1, but it has kind k2 in the conclusion of
the rule. So we do a kind-fixing substitution, replacing (tv1:k1) with
(tv1:k2) |> sym kind_co. This substitution is slightly bizarre, because it
mentions the same name with different kinds, but it *is* well-kinded, noting
that `(tv1:k2) |> sym kind_co` has kind k1.

This all really would work storing just a Name in the ForAllCo. But we can't
add Names to, e.g., VarSets, and there generally is just an impedance mismatch
in a bunch of places. So we use tv1. When we need tv2, we can use
setTyVarKind.

Note [Predicate coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: a~b
How can we coerce between types
   ([c]~a) => [a] -> c
and
   ([c]~b) => [b] -> c
where the equality predicate *itself* differs?

Answer: we simply treat (~) as an ordinary type constructor, so these
types really look like

   ((~) [c] a) -> [a] -> c
   ((~) [c] b) -> [b] -> c

So the coercion between the two is obviously

   ((~) [c] g) -> [g] -> c

Another way to see this to say that we simply collapse predicates to
their representation type (see Type.coreView and Type.predTypeRep).

This collapse is done by mkPredCo; there is no PredCo constructor
in Coercion.  This is important because we need Nth to work on
predicates too:
    Nth 1 ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.

Note [Roles]
~~~~~~~~~~~~
Roles are a solution to the GeneralizedNewtypeDeriving problem, articulated
in #1496. The full story is in docs/core-spec/core-spec.pdf. Also, see
https://gitlab.haskell.org/ghc/ghc/wikis/roles-implementation

Here is one way to phrase the problem:

Given:
newtype Age = MkAge Int
type family F x
type instance F Age = Bool
type instance F Int = Char

This compiles down to:
axAge :: Age ~ Int
axF1 :: F Age ~ Bool
axF2 :: F Int ~ Char

Then, we can make:
(sym (axF1) ; F axAge ; axF2) :: Bool ~ Char

Yikes!

The solution is _roles_, as articulated in "Generative Type Abstraction and
Type-level Computation" (POPL 2010), available at
http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf

The specification for roles has evolved somewhat since that paper. For the
current full details, see the documentation in docs/core-spec. Here are some
highlights.

We label every equality with a notion of type equivalence, of which there are
three options: Nominal, Representational, and Phantom. A ground type is
nominally equivalent only with itself. A newtype (which is considered a ground
type in Haskell) is representationally equivalent to its representation.
Anything is "phantomly" equivalent to anything else. We use "N", "R", and "P"
to denote the equivalences.

The axioms above would be:
axAge :: Age ~R Int
axF1 :: F Age ~N Bool
axF2 :: F Age ~N Char

Then, because transitivity applies only to coercions proving the same notion
of equivalence, the above construction is impossible.

However, there is still an escape hatch: we know that any two types that are
nominally equivalent are representationally equivalent as well. This is what
the form SubCo proves -- it "demotes" a nominal equivalence into a
representational equivalence. So, it would seem the following is possible:

sub (sym axF1) ; F axAge ; sub axF2 :: Bool ~R Char   -- WRONG

What saves us here is that the arguments to a type function F, lifted into a
coercion, *must* prove nominal equivalence. So, (F axAge) is ill-formed, and
we are safe.

Roles are attached to parameters to TyCons. When lifting a TyCon into a
coercion (through TyConAppCo), we need to ensure that the arguments to the
TyCon respect their roles. For example:

data T a b = MkT a (F b)

If we know that a1 ~R a2, then we know (T a1 b) ~R (T a2 b). But, if we know
that b1 ~R b2, we know nothing about (T a b1) and (T a b2)! This is because
the type function F branches on b's *name*, not representation. So, we say
that 'a' has role Representational and 'b' has role Nominal. The third role,
Phantom, is for parameters not used in the type's definition. Given the
following definition

data Q a = MkQ Int

the Phantom role allows us to say that (Q Bool) ~R (Q Char), because we
can construct the coercion Bool ~P Char (using UnivCo).

See the paper cited above for more examples and information.

Note [TyConAppCo roles]
~~~~~~~~~~~~~~~~~~~~~~~
The TyConAppCo constructor has a role parameter, indicating the role at
which the coercion proves equality. The choice of this parameter affects
the required roles of the arguments of the TyConAppCo. To help explain
it, assume the following definition:

  type instance F Int = Bool   -- Axiom axF : F Int ~N Bool
  newtype Age = MkAge Int      -- Axiom axAge : Age ~R Int
  data Foo a = MkFoo a         -- Role on Foo's parameter is Representational

TyConAppCo Nominal Foo axF : Foo (F Int) ~N Foo Bool
  For (TyConAppCo Nominal) all arguments must have role Nominal. Why?
  So that Foo Age ~N Foo Int does *not* hold.

TyConAppCo Representational Foo (SubCo axF) : Foo (F Int) ~R Foo Bool
TyConAppCo Representational Foo axAge       : Foo Age     ~R Foo Int
  For (TyConAppCo Representational), all arguments must have the roles
  corresponding to the result of tyConRoles on the TyCon. This is the
  whole point of having roles on the TyCon to begin with. So, we can
  have Foo Age ~R Foo Int, if Foo's parameter has role R.

  If a Representational TyConAppCo is over-saturated (which is otherwise fine),
  the spill-over arguments must all be at Nominal. This corresponds to the
  behavior for AppCo.

TyConAppCo Phantom Foo (UnivCo Phantom Int Bool) : Foo Int ~P Foo Bool
  All arguments must have role Phantom. This one isn't strictly
  necessary for soundness, but this choice removes ambiguity.

The rules here dictate the roles of the parameters to mkTyConAppCo
(should be checked by Lint).

Note [NthCo and newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype N a = MkN Int
  type role N representational

This yields axiom

  NTCo:N :: forall a. N a ~R Int

We can then build

  co :: forall a b. N a ~R N b
  co = NTCo:N a ; sym (NTCo:N b)

for any `a` and `b`. Because of the role annotation on N, if we use
NthCo, we'll get out a representational coercion. That is:

  NthCo r 0 co :: forall a b. a ~R b

Yikes! Clearly, this is terrible. The solution is simple: forbid
NthCo to be used on newtypes if the internal coercion is representational.

This is not just some corner case discovered by a segfault somewhere;
it was discovered in the proof of soundness of roles and described
in the "Safe Coercions" paper (ICFP '14).

Note [NthCo Cached Roles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Why do we cache the role of NthCo in the NthCo constructor?
Because computing role(Nth i co) involves figuring out that

  co :: T tys1 ~ T tys2

using coercionKind, and finding (coercionRole co), and then looking
at the tyConRoles of T. Avoiding bad asymptotic behaviour here means
we have to compute the kind and role of a coercion simultaneously,
which makes the code complicated and inefficient.

This only happens for NthCo. Caching the role solves the problem, and
allows coercionKind and coercionRole to be simple.

See #11735

Note [InstCo roles]
~~~~~~~~~~~~~~~~~~~
Here is (essentially) the typing rule for InstCo:

g :: (forall a. t1) ~r (forall a. t2)
w :: s1 ~N s2
------------------------------- InstCo
InstCo g w :: (t1 [a |-> s1]) ~r (t2 [a |-> s2])

Note that the Coercion w *must* be nominal. This is necessary
because the variable a might be used in a "nominal position"
(that is, a place where role inference would require a nominal
role) in t1 or t2. If we allowed w to be representational, we
could get bogus equalities.

A more nuanced treatment might be able to relax this condition
somewhat, by checking if t1 and/or t2 use their bound variables
in nominal ways. If not, having w be representational is OK.


%************************************************************************
%*                                                                      *
                UnivCoProvenance
%*                                                                      *
%************************************************************************

A UnivCo is a coercion whose proof does not directly express its role
and kind (indeed for some UnivCos, like UnsafeCoerceProv, there /is/
no proof).

The different kinds of UnivCo are described by UnivCoProvenance.  Really
each is entirely separate, but they all share the need to represent their
role and kind, which is done in the UnivCo constructor.

-}

-- | For simplicity, we have just one UnivCo that represents a coercion from
-- some type to some other type, with (in general) no restrictions on the
-- type. The UnivCoProvenance specifies more exactly what the coercion really
-- is and why a program should (or shouldn't!) trust the coercion.
-- It is reasonable to consider each constructor of 'UnivCoProvenance'
-- as a totally independent coercion form; their only commonality is
-- that they don't tell you what types they coercion between. (That info
-- is in the 'UnivCo' constructor of 'Coercion'.
data UnivCoProvenance
  = UnsafeCoerceProv   -- ^ From @unsafeCoerce#@. These are unsound.

  | PhantomProv KindCoercion -- ^ See Note [Phantom coercions]. Only in Phantom
                             -- roled coercions

  | ProofIrrelProv KindCoercion  -- ^ From the fact that any two coercions are
                                 --   considered equivalent. See Note [ProofIrrelProv].
                                 -- Can be used in Nominal or Representational coercions

  | PluginProv String  -- ^ From a plugin, which asserts that this coercion
                       --   is sound. The string is for the use of the plugin.

  deriving Data.Data

instance Outputable UnivCoProvenance where
  ppr UnsafeCoerceProv   = text "(unsafeCoerce#)"
  ppr (PhantomProv _)    = text "(phantom)"
  ppr (ProofIrrelProv _) = text "(proof irrel.)"
  ppr (PluginProv str)   = parens (text "plugin" <+> brackets (text str))

-- | A coercion to be filled in by the type-checker. See Note [Coercion holes]
data CoercionHole
  = CoercionHole { ch_co_var :: CoVar
                       -- See Note [CoercionHoles and coercion free variables]

                 , ch_ref    :: IORef (Maybe Coercion)
                 }

coHoleCoVar :: CoercionHole -> CoVar
coHoleCoVar = ch_co_var

setCoHoleCoVar :: CoercionHole -> CoVar -> CoercionHole
setCoHoleCoVar h cv = h { ch_co_var = cv }

instance Data.Data CoercionHole where
  -- don't traverse?
  toConstr _   = abstractConstr "CoercionHole"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "CoercionHole"

instance Outputable CoercionHole where
  ppr (CoercionHole { ch_co_var = cv }) = braces (ppr cv)


{- Note [Phantom coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
     data T a = T1 | T2
Then we have
     T s ~R T t
for any old s,t. The witness for this is (TyConAppCo T Rep co),
where (co :: s ~P t) is a phantom coercion built with PhantomProv.
The role of the UnivCo is always Phantom.  The Coercion stored is the
(nominal) kind coercion between the types
   kind(s) ~N kind (t)

Note [Coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~
During typechecking, constraint solving for type classes works by
  - Generate an evidence Id,  d7 :: Num a
  - Wrap it in a Wanted constraint, [W] d7 :: Num a
  - Use the evidence Id where the evidence is needed
  - Solve the constraint later
  - When solved, add an enclosing let-binding  let d7 = .... in ....
    which actually binds d7 to the (Num a) evidence

For equality constraints we use a different strategy.  See Note [The
equality types story] in TysPrim for background on equality constraints.
  - For /boxed/ equality constraints, (t1 ~N t2) and (t1 ~R t2), it's just
    like type classes above. (Indeed, boxed equality constraints *are* classes.)
  - But for /unboxed/ equality constraints (t1 ~R# t2) and (t1 ~N# t2)
    we use a different plan

For unboxed equalities:
  - Generate a CoercionHole, a mutable variable just like a unification
    variable
  - Wrap the CoercionHole in a Wanted constraint; see TcRnTypes.TcEvDest
  - Use the CoercionHole in a Coercion, via HoleCo
  - Solve the constraint later
  - When solved, fill in the CoercionHole by side effect, instead of
    doing the let-binding thing

The main reason for all this is that there may be no good place to let-bind
the evidence for unboxed equalities:

  - We emit constraints for kind coercions, to be used to cast a
    type's kind. These coercions then must be used in types. Because
    they might appear in a top-level type, there is no place to bind
    these (unlifted) coercions in the usual way.

  - A coercion for (forall a. t1) ~ (forall a. t2) will look like
       forall a. (coercion for t1~t2)
    But the coercion for (t1~t2) may mention 'a', and we don't have
    let-bindings within coercions.  We could add them, but coercion
    holes are easier.

  - Moreover, nothing is lost from the lack of let-bindings. For
    dicionaries want to achieve sharing to avoid recomoputing the
    dictionary.  But coercions are entirely erased, so there's little
    benefit to sharing. Indeed, even if we had a let-binding, we
    always inline types and coercions at every use site and drop the
    binding.

Other notes about HoleCo:

 * INVARIANT: CoercionHole and HoleCo are used only during type checking,
   and should never appear in Core. Just like unification variables; a Type
   can contain a TcTyVar, but only during type checking. If, one day, we
   use type-level information to separate out forms that can appear during
   type-checking vs forms that can appear in core proper, holes in Core will
   be ruled out.

 * See Note [CoercionHoles and coercion free variables]

 * Coercion holes can be compared for equality like other coercions:
   by looking at the types coerced.


Note [CoercionHoles and coercion free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why does a CoercionHole contain a CoVar, as well as reference to
fill in?  Because we want to treat that CoVar as a free variable of
the coercion.  See #14584, and Note [What prevents a
constraint from floating] in TcSimplify, item (4):

        forall k. [W] co1 :: t1 ~# t2 |> co2
                  [W] co2 :: k ~# *

Here co2 is a CoercionHole. But we /must/ know that it is free in
co1, because that's all that stops it floating outside the
implication.


Note [ProofIrrelProv]
~~~~~~~~~~~~~~~~~~~~~
A ProofIrrelProv is a coercion between coercions. For example:

  data G a where
    MkG :: G Bool

In core, we get

  G :: * -> *
  MkG :: forall (a :: *). (a ~ Bool) -> G a

Now, consider 'MkG -- that is, MkG used in a type -- and suppose we want
a proof that ('MkG a1 co1) ~ ('MkG a2 co2). This will have to be

  TyConAppCo Nominal MkG [co3, co4]
  where
    co3 :: co1 ~ co2
    co4 :: a1 ~ a2

Note that
  co1 :: a1 ~ Bool
  co2 :: a2 ~ Bool

Here,
  co3 = UnivCo (ProofIrrelProv co5) Nominal (CoercionTy co1) (CoercionTy co2)
  where
    co5 :: (a1 ~ Bool) ~ (a2 ~ Bool)
    co5 = TyConAppCo Nominal (~#) [<*>, <*>, co4, <Bool>]
-}


{- *********************************************************************
*                                                                      *
                   typeSize, coercionSize
*                                                                      *
********************************************************************* -}

-- NB: We put typeSize/coercionSize here because they are mutually
--     recursive, and have the CPR property.  If we have mutual
--     recursion across a hi-boot file, we don't get the CPR property
--     and these functions allocate a tremendous amount of rubbish.
--     It's not critical (because typeSize is really only used in
--     debug mode, but I tripped over an example (T5642) in which
--     typeSize was one of the biggest single allocators in all of GHC.
--     And it's easy to fix, so I did.

-- NB: typeSize does not respect `eqType`, in that two types that
--     are `eqType` may return different sizes. This is OK, because this
--     function is used only in reporting, not decision-making.

typeSize :: Type -> Int
typeSize (LitTy {})                 = 1
typeSize (TyVarTy {})               = 1
typeSize (AppTy t1 t2)              = typeSize t1 + typeSize t2
typeSize (FunTy _ t1 t2)            = typeSize t1 + typeSize t2
typeSize (ForAllTy (Bndr tv _) t)   = typeSize (varType tv) + typeSize t
typeSize (TyConApp _ ts)            = 1 + sum (map typeSize ts)
typeSize (CastTy ty co)             = typeSize ty + coercionSize co
typeSize (CoercionTy co)            = coercionSize co

coercionSize :: Coercion -> Int
coercionSize (Refl ty)             = typeSize ty
coercionSize (GRefl _ ty MRefl)    = typeSize ty
coercionSize (GRefl _ ty (MCo co)) = 1 + typeSize ty + coercionSize co
coercionSize (TyConAppCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (AppCo co arg)      = coercionSize co + coercionSize arg
coercionSize (ForAllCo _ h co)   = 1 + coercionSize co + coercionSize h
coercionSize (FunCo _ co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (CoVarCo _)         = 1
coercionSize (HoleCo _)          = 1
coercionSize (AxiomInstCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (UnivCo p _ t1 t2)  = 1 + provSize p + typeSize t1 + typeSize t2
coercionSize (SymCo co)          = 1 + coercionSize co
coercionSize (TransCo co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (NthCo _ _ co)      = 1 + coercionSize co
coercionSize (LRCo  _ co)        = 1 + coercionSize co
coercionSize (InstCo co arg)     = 1 + coercionSize co + coercionSize arg
coercionSize (KindCo co)         = 1 + coercionSize co
coercionSize (SubCo co)          = 1 + coercionSize co
coercionSize (AxiomRuleCo _ cs)  = 1 + sum (map coercionSize cs)

provSize :: UnivCoProvenance -> Int
provSize UnsafeCoerceProv    = 1
provSize (PhantomProv co)    = 1 + coercionSize co
provSize (ProofIrrelProv co) = 1 + coercionSize co
provSize (PluginProv _)      = 1
