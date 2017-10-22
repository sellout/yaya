-- | Definitions and instances that use direct recursion.
module Yaya.Unsafe.Control where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Data.Function
import Data.Functor.Compose

import Yaya
import Yaya.Control
import Yaya.Data

anaM
  :: (Monad m, Cursive t f, Corecursive t f, Traversable f)
  => CoalgebraM m f a
  -> a
  -> m t
anaM = hyloM $ pure . embed

ganaM
  :: (Monad m, Monad n, Traversable n, Cursive t f, Corecursive t f, Traversable f)
  => DistributiveLaw n f
  -> GCoalgebraM m n f a
  -> a
  -> m t
ganaM k ψ = anaM (fmap (fmap join . k) . traverse ψ) . pure

mutu
  :: (Cursive t f, Recursive t f, Functor f)
  => GAlgebra ((,) a) f b
  -> GAlgebra ((,) b) f a
  -> t
  -> a
mutu φ' φ = φ . fmap (mutu φ φ' &&& mutu φ' φ) . project

gprepro
  :: (Cursive t f, Recursive t f, Functor f, Comonad w)
  => DistributiveLaw f w
  -> GAlgebra w f a
  -> (forall a. f a -> f a)
  -> t
  -> a
gprepro k φ e =
  extract
  . hylo (degeneralizeAlgebra k φ) (fmap (cata (embed . e)) . project)

gpostpro
  :: (Cursive t f, Corecursive t f, Functor f, Monad m)
  => DistributiveLaw m f
  -> (forall a. f a -> f a)
  -> GCoalgebra m f a
  -> a
  -> t
gpostpro k e ψ =
  hylo (embed . fmap (ana (e . project))) (degeneralizeCoalgebra k ψ) . pure

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
  -> a
  -> b
ghylo w m φ ψ =
  extract
    . hylo (degeneralizeAlgebra w φ) (degeneralizeCoalgebra m ψ)
    . pure

hyloM
  :: (Monad m, Traversable f)
  => AlgebraM m f b
  -> CoalgebraM m f a
  -> a
  -> m b
hyloM φ ψ = hylo (φ <=< join . fmap sequenceA . getCompose) (Compose . ψ)

ghyloM
  :: (Comonad w, Traversable w, Monad m, Traversable f, Monad n, Traversable n)
  => DistributiveLaw f w
  -> DistributiveLaw n f
  -> GAlgebraM m w f b
  -> GCoalgebraM m n f a
  -> a
  -> m b
ghyloM w n φ ψ =
  fmap extract
    . hyloM (traverse φ . w . fmap duplicate) (fmap (fmap join . n) . traverse ψ)
    . pure

stream'
  :: (Cursive t e, Cursive u f, Functor f)
  => CoalgebraM Maybe f b
  -> (b -> ((b -> b, t) -> u) -> e t -> u)
  -> b
  -> t
  -> u
stream' ψ f = go
  where
    go c x =
      maybe (f c (uncurry go . ((&) c *** id)) $ project x)
            (embed . fmap (flip go x))
            $ ψ c

-- | Gibbons’ metamorphism. It lazily folds a (necessarily infinite) value,
--   incrementally re-expanding that value into some new representation.
streamAna
  :: (Cursive t e, Cursive u f, Functor f)
  => CoalgebraM Maybe f b
  -> AlgebraM ((,) (b -> b)) e t
  -> b
  -> t
  -> u
streamAna ψ φ = stream' ψ $ \c f -> f . φ

-- | Another form of Gibbons’ metamorphism. This one can be applied to non-
--   infinite inputs and takes an additional “flushing” coalgebra to be applied
--   after all the input has been consumed.
streamGApo
  :: (Cursive t e, Cursive u f, Corecursive u f, Functor f)
  => Coalgebra f b
  -> CoalgebraM Maybe f b
  -> (e t -> Maybe (b -> b, t))
  -> b
  -> t
  -> u
streamGApo ψ' ψ φ = stream' ψ $ \c f -> maybe (ana ψ' c) f . φ
