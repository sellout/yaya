-- | Definitions and instances that use direct recursion.
module Yaya.Unsafe.Control where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Data.Functor.Compose

import Yaya
import Yaya.Control
import Yaya.Data

-- ganaM
--   :: (Monad m, Corecursive t f, Traversable f, Monad n)
--   => DistributiveLaw n f
--   -> GCoalgebraM m n f a
--   -> a
--   -> m t
-- ganaM k = ghyloM distCata k $ pure . embed

mutu
  :: (Recursive t f, Functor f)
  => GAlgebra ((,) a) f b
  -> GAlgebra ((,) b) f a
  -> t
  -> a
mutu φ' φ = φ . fmap (mutu φ φ' &&& mutu φ' φ) . project

gprepro
  :: (Recursive t f, Functor f, Comonad w)
  => DistributiveLaw f w
  -> (forall a. f a -> f a)
  -> GAlgebra w f a
  -> t
  -> a
gprepro k e φ = extract . go
  where
    go = fmap φ . k . fmap (duplicate . go . cata (embed . e)) . project

gpostpro
  :: (Corecursive t f, Functor f, Monad m)
  => DistributiveLaw m f
  -> GCoalgebra m f a
  -> (forall a. f a -> f a)
  -> a
  -> t
gpostpro k ψ e = go . pure
  where
    go = embed . fmap (ana (e . project) . go . join) . k . liftM ψ

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
    . hylo (fmap φ . w . fmap duplicate)
           (fmap join . m . fmap ψ)
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
