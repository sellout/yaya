-- | Definitions and instances that use direct recursion, which (because of
--   laziness) can lead to non-termination.
module Yaya.Unsafe.Fold where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Data.Either.Combinators
import Data.Function
import Data.Functor.Compose
import Data.Functor.Identity

import Yaya.Fold

-- | This can’t be implemented in a total fashion. There is a _similar_ approach
--   that can be total – with `ψ :: CoalgebraM m f a`, `ana (Compose . ψ)`
--   results in something like `Nu (Compose m f)` which is akin to an effectful
--   stream.
anaM :: (Monad m, Steppable t f, Traversable f) => CoalgebraM m f a -> a -> m t
anaM = hyloM (pure . embed)

ganaM
  :: (Monad m, Monad n, Traversable n, Steppable t f, Traversable f)
  => DistributiveLaw n f
  -> GCoalgebraM m n f a
  -> a -> m t
ganaM k ψ = anaM (lowerCoalgebraM k ψ) . pure

-- | Fusion of an 'ana' and 'cata'.
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo φ ψ = go
  where
    go = φ . fmap go . ψ

ghylo
  :: (Comonad w, Monad m, Functor f)
  => DistributiveLaw f w
  -> DistributiveLaw m f
  -> GAlgebra w f b
  -> GCoalgebra m f a
  -> a -> b
ghylo w m φ ψ =
  extract . hylo (lowerAlgebra w φ) (lowerCoalgebra m ψ) . pure

hyloM
  :: (Monad m, Traversable f)
  => AlgebraM m f b
  -> CoalgebraM m f a
  -> a -> m b
hyloM φ ψ = hylo (φ <=< sequenceA <=< getCompose) (Compose . ψ)

ghyloM
  :: (Comonad w, Traversable w, Monad m, Traversable f, Monad n, Traversable n)
  => DistributiveLaw f w
  -> DistributiveLaw n f
  -> GAlgebraM m w f b
  -> GCoalgebraM m n f a
  -> a -> m b
ghyloM w n φ ψ =
  fmap extract . hyloM (lowerAlgebraM w φ) (lowerCoalgebraM n ψ) . pure

stream'
  :: (Steppable t e, Steppable u f, Functor f)
  => CoalgebraM Maybe f b
  -> (b -> ((b -> b, t) -> u) -> e t -> u)
  -> b
  -> t -> u
stream' ψ f = go
  where
    go c x =
      maybe (f c (uncurry go . ((&) c *** id)) $ project x)
            (embed . fmap (flip go x))
            $ ψ c

-- | Gibbons’ metamorphism. It lazily folds a (necessarily infinite) value,
--   incrementally re-expanding that value into some new representation.
streamAna
  :: (Steppable t e, Steppable u f, Functor f)
  => CoalgebraM Maybe f b
  -> AlgebraM ((,) (b -> b)) e t
  -> b
  -> t -> u
streamAna ψ φ = stream' ψ $ \c f -> f . φ

-- | Another form of Gibbons’ metamorphism. This one can be applied to non-
--   infinite inputs and takes an additional “flushing” coalgebra to be applied
--   after all the input has been consumed.
streamGApo
  :: (Steppable t e, Steppable u f, Corecursive u f, Functor f)
  => Coalgebra f b
  -> CoalgebraM Maybe f b
  -> (e t -> Maybe (b -> b, t))
  -> b
  -> t -> u
streamGApo ψ' ψ φ = stream' ψ $ \c f -> maybe (ana ψ' c) f . φ

corecursivePrism
  :: (Steppable t f, Recursive t f, Corecursive t f, Traversable f)
  => CoalgebraPrism f a
  -> Prism' a t
corecursivePrism alg = prism (cata (review alg)) (anaM (matching alg))
