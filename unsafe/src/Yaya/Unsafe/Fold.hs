-- | Definitions and instances that use direct recursion, which (because of
--   laziness) can lead to non-termination.
module Yaya.Unsafe.Fold where

import Control.Arrow
import Control.Comonad
import Control.Lens
import Control.Monad
import Data.Functor.Compose

import Yaya.Fold

-- | This can’t be implemented in a total fashion. There is a _similar_ approach
--   that can be total – with `ψ :: CoalgebraM (->) m f a`, `ana (Compose . ψ)`
--   results in something like `Nu (Compose m f)` which is akin to an effectful
--   stream.
anaM :: (Monad m, Steppable (->) t f, Traversable f) => CoalgebraM (->) m f a -> a -> m t
anaM = hyloM (pure . embed)

ganaM
  :: (Monad m, Monad n, Traversable n, Steppable (->) t f, Traversable f)
  => DistributiveLaw (->) n f
  -> GCoalgebraM (->) m n f a
  -> a -> m t
ganaM k ψ = anaM (lowerCoalgebraM k ψ) . pure

-- | Fusion of an 'ana' and 'cata'.
hylo :: Functor f => Algebra (->) f b -> Coalgebra (->) f a -> a -> b
hylo φ ψ = go
  where
    go = φ . fmap go . ψ

ghylo
  :: (Comonad w, Monad m, Functor f)
  => DistributiveLaw (->) f w
  -> DistributiveLaw (->) m f
  -> GAlgebra (->) w f b
  -> GCoalgebra (->) m f a
  -> a -> b
ghylo w m φ ψ =
  extract . hylo (lowerAlgebra w φ) (lowerCoalgebra m ψ) . pure

hyloM
  :: (Monad m, Traversable f)
  => AlgebraM (->) m f b
  -> CoalgebraM (->) m f a
  -> a -> m b
hyloM φ ψ = hylo (φ <=< sequenceA <=< getCompose) (Compose . ψ)

ghyloM
  :: (Comonad w, Traversable w, Monad m, Traversable f, Monad n, Traversable n)
  => DistributiveLaw (->) f w
  -> DistributiveLaw (->) n f
  -> GAlgebraM (->) m w f b
  -> GCoalgebraM (->) m n f a
  -> a -> m b
ghyloM w n φ ψ =
  fmap extract . hyloM (lowerAlgebraM w φ) (lowerCoalgebraM n ψ) . pure

stream'
  :: (Projectable (->) t f, Steppable (->) u g, Functor g)
  => CoalgebraM (->) Maybe g b
  -> (b -> ((b -> b, t) -> u) -> f t -> u)
  -> b
  -> t -> u
stream' ψ f = go
  where
    go c x =
      maybe (f c (uncurry go . ((&) c *** id)) (project x))
            (embed . fmap (flip go x))
            (ψ c)

-- | Gibbons’ metamorphism. It lazily folds a (necessarily infinite) value,
--   incrementally re-expanding that value into some new representation.
streamAna
  :: (Projectable (->) t f, Steppable (->) u g, Functor g)
  => CoalgebraM (->) Maybe g b
  -> AlgebraM (->) ((,) (b -> b)) f t
  -> b
  -> t -> u
streamAna ψ φ = stream' ψ (\_ f -> f . φ)

-- | Another form of Gibbons’ metamorphism. This one can be applied to non-
--   infinite inputs and takes an additional “flushing” coalgebra to be applied
--   after all the input has been consumed.
streamGApo
  :: (Projectable (->) t f, Steppable (->) u g, Corecursive (->) u g, Functor g)
  => Coalgebra (->) g b
  -> CoalgebraM (->) Maybe g b
  -> (f t -> Maybe (b -> b, t))
  -> b
  -> t -> u
streamGApo ψ' ψ φ = stream' ψ (\c f -> maybe (ana ψ' c) f . φ)

corecursivePrism
  :: (Steppable (->) t f, Recursive (->) t f, Corecursive (->) t f, Traversable f)
  => CoalgebraPrism f a
  -> Prism' a t
corecursivePrism alg = prism (cata (review alg)) (anaM (matching alg))
