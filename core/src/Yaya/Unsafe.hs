-- | Definitions and instances that use direct recursion.
module Yaya.Unsafe where

import Control.Arrow
import Control.Comonad
import Control.Monad
import Data.Functor.Yoneda

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

data Fix f = Fix { unFix :: f (Fix f) }

instance Cursive (Fix f) f where
  embed = Fix
  project = unFix

instance Functor f => Recursive (Fix f) f where
  cata φ = hylo φ project

instance Functor f => Corecursive (Fix f) f where
  ana ψ = hylo embed ψ

instance Functor f => Corecursive (Mu f) f where
  ana ψ = hylo embed ψ

instance Functor f => Recursive (Nu f) f where
  cata φ = hylo φ project

-- List instance

data XNor a b = None | Both a b

instance Functor (XNor a) where
  fmap _ None = None
  fmap f (Both a b) = Both a (f b)

instance Cursive [a] (XNor a) where
  embed None = []
  embed (Both h t) = h : t
  project [] = None
  project (h : t) = Both h t

instance Recursive [a] (XNor a) where
  cata φ = hylo φ project

instance Corecursive [a] (XNor a) where
  ana ψ = hylo embed ψ
