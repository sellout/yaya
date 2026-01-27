{-# LANGUAGE Safe #-}

module Yaya.Lens
  ( AlgebraLens,
    AlgebraLens',
    BialgebraIso,
    BialgebraIso',
    CoalgebraPrism,
    CoalgebraPrism',
    birecursiveIso,
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
import "base" Data.Traversable (Traversable, sequenceA)
import "lens" Control.Lens
  ( Iso,
    Lens,
    Prism,
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

-- | An isomorphism between an @`Algebra` (->) f a@ and a @`Coalgebra` (->) g b@.
type BialgebraIso f g a b = Iso (f a) (g b) a b

type BialgebraIso' f a = BialgebraIso f f a a

type AlgebraLens f g a b = Lens (f a) (g b) a b

type AlgebraLens' f a = AlgebraLens f f a a

-- | A prism between an @`AlgebraM` (->) (`Either` (f a)) f a@ and a
--   @`Coalgebra` g b@.
type CoalgebraPrism f g a b = Prism (f a) (g b) a b

type CoalgebraPrism' f a = CoalgebraPrism f f a a

-- | The isomorphism between `embed` and `project`.
steppableIso ::
  (Steppable (->) t f, Projectable (->) u g) => BialgebraIso f g t u
steppableIso = iso embed project

-- | The isomorphism between `cata` and `ana` as a function from an isomorphism
--   between (co)algebras to an isomorphism between the (co)recursive structure
--   and its folded value.
birecursiveIso ::
  (Recursive (->) s f, Corecursive (->) t f) =>
  BialgebraIso' f a ->
  Iso s t a a
birecursiveIso φ = iso (cata (view φ)) (ana (review φ))

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
