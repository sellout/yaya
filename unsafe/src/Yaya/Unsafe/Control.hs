-- | Definitions and instances that use direct recursion.
module Yaya.Unsafe.Control where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Control.Monad.Free

import Yaya
import Yaya.Control
import Yaya.Data

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
