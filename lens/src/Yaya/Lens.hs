{-# LANGUAGE Safe #-}

module Yaya.Lens
  ( AlgebraPrism,
    BialgebraIso,
    CoalgebraPrism,
    birecursiveIso,
    recursivePrism,
    steppableIso,
  )
where

import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (first)
import "base" Data.Function (const, ($))
import "base" Data.Traversable (Traversable, sequenceA)
import "lens" Control.Lens (Iso', Prism', iso, matching, prism, review, view)
import "yaya" Yaya.Fold
  ( Corecursive,
    Recursive,
    Steppable,
    ana,
    cata,
    embed,
    project,
  )

type BialgebraIso f a = Iso' (f a) a

type AlgebraPrism f a = Prism' (f a) a

type CoalgebraPrism f a = Prism' a (f a)

steppableIso :: (Steppable (->) t f) => BialgebraIso f t
steppableIso = iso embed project

birecursiveIso ::
  (Recursive (->) t f, Corecursive (->) t f) =>
  BialgebraIso f a ->
  Iso' t a
birecursiveIso alg = iso (cata (view alg)) (ana (review alg))

recursivePrism ::
  (Recursive (->) t f, Corecursive (->) t f, Traversable f) =>
  AlgebraPrism f a ->
  Prism' t a
recursivePrism alg =
  prism
    (ana (review alg))
    (\t -> first (const t) $ cata (matching alg <=< sequenceA) t)
