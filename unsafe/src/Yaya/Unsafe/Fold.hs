{-# LANGUAGE Safe #-}

-- | Definitions and instances that use direct recursion, which (because of
--   laziness) can lead to non-termination.
module Yaya.Unsafe.Fold
  ( anaM,
    corecursivePrism,
    ganaM,
    ghylo,
    ghyloM,
    hylo,
    hyloM,
    stream',
    streamAna,
    streamGApo,
    unsafeAna,
    unsafeCata,
  )
where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category ((.)))
import "base" Control.Monad (Monad, (<=<))
import "base" Data.Bifunctor (Bifunctor (first))
import "base" Data.Function (flip)
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Compose (Compose (Compose, getCompose))
import "base" Data.Traversable (Traversable (sequenceA))
import "comonad" Control.Comonad (Comonad (extract))
import "lens" Control.Lens (Prism', matching, prism, review, (&))
import "yaya" Yaya.Fold
  ( Algebra,
    AlgebraM,
    Coalgebra,
    CoalgebraM,
    CoalgebraPrism,
    Corecursive (ana),
    DistributiveLaw,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    GCoalgebraM,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    lowerAlgebra,
    lowerAlgebraM,
    lowerCoalgebra,
    lowerCoalgebraM,
  )
import "yaya" Yaya.Pattern (Maybe, Pair, maybe, uncurry)

-- | Instances leak transitively, so while "Yaya.Unsafe.Fold.Instances" exists,
--   it should only be used when it is unavoidable. If you are explicitly
--   folding a structure unsafely, use this function instead of importing that
--   module.
unsafeAna :: (Steppable (->) t f, Functor f) => Coalgebra (->) f a -> a -> t
unsafeAna = hylo embed

-- | Instances leak transitively, so while "Yaya.Unsafe.Fold.Instances" exists,
--   it should only be used when it is unavoidable. If you are explicitly
--   unfolding a structure unsafely, use this function instead of importing that
--   module.
--
--   Should one prefer `unsafeAna` or `unsafeCata` in cases where both are
--   applicable?
-- - one may provide weaker constraints than the other in certain cases (e.g.,
--   on its own, `unsafeCata` only requires `Projectable` on the source, but
--  `unsafeAna` requires `Steppable` on the target. Depending on what other
--   constraints already exist on the function, either one may ultimately be
--   less constrained.
-- - they may fail differently: `unsafeCata` (folding a potentially-infinite
--   structure) is likely to result in non-termination, whereas `unsafeAna`
--   (building a potentially-infinite structure strictly) is likely to use up
--   the memory or overflow the stack.
unsafeCata :: (Projectable (->) t f, Functor f) => Algebra (->) f a -> t -> a
unsafeCata = flip hylo project

-- | This can’t be implemented in a total fashion. There is a /similar/ approach
--   that can be total – with `ψ :: CoalgebraM (->) m f a`, `ana (Compose . ψ)`
--   results in something like `Nu (Compose m f)` which is akin to an effectful
--   stream.
anaM ::
  (Monad m, Steppable (->) t f, Traversable f) =>
  CoalgebraM (->) m f a ->
  a ->
  m t
anaM = hyloM (pure . embed)

ganaM ::
  (Monad m, Monad n, Traversable n, Steppable (->) t f, Traversable f) =>
  DistributiveLaw (->) n f ->
  GCoalgebraM (->) m n f a ->
  a ->
  m t
ganaM k ψ = anaM (lowerCoalgebraM k ψ) . pure

-- | Fusion of an 'ana' and 'cata'.
hylo :: (Functor f) => Algebra (->) f b -> Coalgebra (->) f a -> a -> b
hylo φ ψ = go
  where
    go = φ . fmap go . ψ

ghylo ::
  (Comonad w, Monad m, Functor f) =>
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) m f ->
  GAlgebra (->) w f b ->
  GCoalgebra (->) m f a ->
  a ->
  b
ghylo w m φ ψ =
  extract . hylo (lowerAlgebra w φ) (lowerCoalgebra m ψ) . pure

hyloM ::
  (Monad m, Traversable f) =>
  AlgebraM (->) m f b ->
  CoalgebraM (->) m f a ->
  a ->
  m b
hyloM φ ψ = hylo (φ <=< sequenceA <=< getCompose) (Compose . ψ)

ghyloM ::
  (Comonad w, Traversable w, Monad m, Traversable f, Monad n, Traversable n) =>
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) n f ->
  GAlgebraM (->) m w f b ->
  GCoalgebraM (->) m n f a ->
  a ->
  m b
ghyloM w n φ ψ =
  fmap extract . hyloM (lowerAlgebraM w φ) (lowerCoalgebraM n ψ) . pure

stream' ::
  (Projectable (->) t f, Steppable (->) u g, Functor g) =>
  CoalgebraM (->) Maybe g b ->
  (b -> (Pair (b -> b) t -> u) -> f t -> u) ->
  b ->
  t ->
  u
stream' ψ f = go
  where
    go c x =
      maybe
        (f c (uncurry go . first (c &)) (project x))
        (embed . fmap (`go` x))
        (ψ c)

-- | Gibbons’ metamorphism. It lazily folds a (necessarily infinite) value,
--   incrementally re-expanding that value into some new representation.
--
--  __NB__: See https://gist.github.com/sellout/4709e723cb649110af00217486c4466b
--          for some commentary and explanation.
streamAna ::
  (Projectable (->) t f, Steppable (->) u g, Functor g) =>
  CoalgebraM (->) Maybe g b ->
  AlgebraM (->) (Pair (b -> b)) f t ->
  b ->
  t ->
  u
streamAna process accum = stream' process (\_ f -> f . accum)

-- | Another form of Gibbons’ metamorphism. This one can be applied to non-
--   infinite inputs and takes an additional “flushing” coalgebra to be applied
--   after all the input has been consumed.
--
--  __NB__: See https://gist.github.com/sellout/4709e723cb649110af00217486c4466b
--          for some commentary and explanation.
streamGApo ::
  (Projectable (->) t f, Steppable (->) u g, Corecursive (->) u g, Functor g) =>
  Coalgebra (->) g b ->
  CoalgebraM (->) Maybe g b ->
  (f t -> Maybe (Pair (b -> b) t)) ->
  b ->
  t ->
  u
streamGApo flush process accum =
  stream' process (\c f -> maybe (ana flush c) f . accum)

corecursivePrism ::
  (Steppable (->) t f, Recursive (->) t f, Traversable f) =>
  CoalgebraPrism f a ->
  Prism' a t
corecursivePrism alg = prism (cata (review alg)) (anaM (matching alg))
