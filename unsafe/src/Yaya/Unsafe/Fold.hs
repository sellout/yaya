-- | Definitions and instances that use direct recursion, which (because of
--   laziness) can lead to non-termination.
module Yaya.Unsafe.Fold where

import Control.Applicative (Applicative (..))
import Control.Category (Category (..))
import Control.Comonad (Comonad (..))
import Control.Lens (Prism', matching, prism, review, (&))
import Control.Monad (Monad, (<=<))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (Functor (..))
import Data.Functor.Compose (Compose (..))
import Data.Traversable (Traversable (..))
import Yaya.Fold
  ( Algebra,
    AlgebraM,
    Coalgebra,
    CoalgebraM,
    CoalgebraPrism,
    Corecursive (..),
    DistributiveLaw,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    GCoalgebraM,
    Projectable (..),
    Recursive (..),
    Steppable (..),
    lowerAlgebra,
    lowerAlgebraM,
    lowerCoalgebra,
    lowerCoalgebraM,
  )
import Yaya.Pattern (Maybe, Pair, maybe, uncurry)

-- | This can’t be implemented in a total fashion. There is a _similar_ approach
--   that can be total – with `ψ :: CoalgebraM (->) m f a`, `ana (Compose . ψ)`
--   results in something like `Nu (Compose m f)` which is akin to an effectful
--   stream.
anaM :: (Monad m, Steppable (->) t f, Traversable f) => CoalgebraM (->) m f a -> a -> m t
anaM = hyloM (pure . embed)

ganaM ::
  (Monad m, Monad n, Traversable n, Steppable (->) t f, Traversable f) =>
  DistributiveLaw (->) n f ->
  GCoalgebraM (->) m n f a ->
  a ->
  m t
ganaM k ψ = anaM (lowerCoalgebraM k ψ) . pure

-- | Fusion of an 'ana' and 'cata'.
hylo :: Functor f => Algebra (->) f b -> Coalgebra (->) f a -> a -> b
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
  (Steppable (->) t f, Recursive (->) t f, Corecursive (->) t f, Traversable f) =>
  CoalgebraPrism f a ->
  Prism' a t
corecursivePrism alg = prism (cata (review alg)) (anaM (matching alg))
