{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
-- "Control.Lens" is only inferred `Safe`.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
#endif

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.0.1
module Yaya.Lens
  ( AlgebraLens,
    AlgebraLens',
    BialgebraIso,
    BialgebraIso',
    CoalgebraPrism,
    CoalgebraPrism',
    anaIso,
    birecursiveIso,
    cataIso,
    folded,
    steppableIso,
    unfolded,
  )
where

import "base" Control.Category ((.))
import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (first)
import "base" Data.Function (const, flip, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Kind (Type)
import "base" Data.Traversable (Traversable, sequenceA)
import "lens" Control.Lens
  ( Iso,
    Iso',
    Lens,
    Prism,
    from,
    iso,
    lens,
    matching,
    prism,
    review,
    set,
    view,
  )
import "yaya" Yaya.Fold
  ( Corecursive,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    embed,
    project,
  )

-- | An isomorphism between an @`Algebra` (->) f a@ and a
--   @`Coalgebra` (->) g b@.
--
-- @since 0.0.1
type BialgebraIso (f :: Type -> Type) (g :: Type -> Type) (a :: Type) (b :: Type) =
  (Iso (f a) (g b) a b :: Type)

-- | An algebra @f a -> a@ together with its inverse coalgebra @a -> f a@.
--
-- @since 0.0.1
type BialgebraIso' (f :: Type -> Type) (a :: Type) = (BialgebraIso f f a a :: Type)

-- | A lens from one layer @f a@ to its carrier @a@, whose getter is an
--   @`Algebra` (->) f a@ and whose setter replaces the focus, yielding a
--   @g b@.
--
-- @since 0.0.1
type AlgebraLens (f :: Type -> Type) (g :: Type -> Type) (a :: Type) (b :: Type) =
  (Lens (f a) (g b) a b :: Type)

-- | A simple `AlgebraLens`, which neither changes the pattern functor nor
--   the carrier.
--
-- @since 0.0.1
type AlgebraLens' (f :: Type -> Type) (a :: Type) = (AlgebraLens f f a a :: Type)

-- | A prism between an @`AlgebraM` (->) (`Either` (g b)) f a@ and a
--   @`Coalgebra` (->) g b@.
--
-- @since 0.0.1
type CoalgebraPrism (f :: Type -> Type) (g :: Type -> Type) (a :: Type) (b :: Type) =
  (Prism (f a) (g b) a b :: Type)

-- | A coalgebra @a -> f a@ together with a partial algebra @f a -> `Maybe` a@
--   that inverts it.
--
-- @since 0.0.1
type CoalgebraPrism' (f :: Type -> Type) (a :: Type) = (CoalgebraPrism f f a a :: Type)

-- | The isomorphism between `embed` and `project`.
--
-- @since 0.0.1
steppableIso ::
  (Steppable (->) t f, Projectable (->) u g) => BialgebraIso f g t u
steppableIso = iso embed project

-- | The isomorphism between `cata` and `ana` as a function from an isomorphism
--   between (co)algebras to an isomorphism between the (co)recursive structure
--   and its folded value.
--
-- @since 0.0.1
birecursiveIso ::
  (Recursive (->) s f, Corecursive (->) t f) =>
  BialgebraIso' f a ->
  Iso s t a a
birecursiveIso φ = iso (cata (view φ)) (ana (review φ))

-- | Lifts an `AlgebraLens'` on one layer to a lens on a whole recursive
--   structure. Its getter is the `cata` of the layer's getter, and its setter
--   rebuilds the structure with `ana`.
--
-- @since 0.0.1
folded ::
  ( Recursive (->) s f,
    Projectable (->) s f,
    Corecursive (->) t f,
    Traversable f
  ) =>
  AlgebraLens' f a ->
  Lens s t a a
folded φ =
  lens (cata (view φ)) (\s -> ana . flip (set φ) $ cata (view φ) <$> project s)

-- | The prism between an unfold and a monadic fold.
--
-- @since 0.0.1
unfolded ::
  ( Recursive (->) s f,
    Projectable (->) s f,
    Corecursive (->) t f,
    Traversable f
  ) =>
  CoalgebraPrism' f a ->
  Prism s t a a
unfolded ψ =
  prism
    (ana $ review ψ)
    (\s -> first (const $ ana project s) $ cata (matching ψ <=< sequenceA) s)

-- | Lift an isomorphism on natural transformations into an isomorphism on
--  `Recursive` structures.
--
--   source: Zanzi in [Monoidal Cafe #beginner-questions](https://discordapp.com/channels/1005220974523846678/1224686592187433082/1225431802068340796)
--
-- @since 0.0.1
cataIso ::
  ( Recursive (->) t f,
    Steppable (->) t f,
    Recursive (->) u g,
    Steppable (->) u g
  ) =>
  (forall a. Iso' (f a) (g a)) ->
  Iso' t u
cataIso nt = iso (cata $ embed . view nt) (cata $ embed . view (from nt))

-- | The dual of `cataIso`.
--
-- @since 0.0.1
anaIso ::
  ( Corecursive (->) t f,
    Projectable (->) t f,
    Corecursive (->) u g,
    Projectable (->) u g
  ) =>
  (forall a. Iso' (f a) (g a)) ->
  Iso' t u
anaIso nt = iso (ana $ view nt . project) (ana $ view (from nt) . project)
