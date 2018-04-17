-- | Definitions and instances that use direct recursion.
--   This contains instances that you might _expect_ to see, but which aren’t
--   actually total. For example, folding a lazy list `[a]` is _not_ guaranteed
--   to terminate. It also currently contains any instance which uses “native”
--   recursion, even if it is safe. These should probably be pulled into a
--   separate madule.
module Yaya.Unsafe.Data where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Control.Monad.Trans.Free
import Data.Functor.Classes

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Unsafe.Control

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Embeddable (Fix f) f where
  embed = Fix

instance Projectable (Fix f) f where
  project = unFix

instance Functor f => Corecursive (Fix f) f where
  ana φ = embed . fmap (ana φ) . φ

-- TODO: reimplement without `hylo`.
instance Corecursive [a] (XNor a) where
  ana = hylo embed

-- TODO: Above here – move to `Yaya.Native` module.

instance Functor f => Recursive (Fix f) f where
  cata = flip hylo project

-- FIXME: Relies on primitive recursion.
instance Eq1 f => Eq (Fix f) where
  a == b = liftEq (==) (project a) (project b)

instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

instance Functor f => Corecursive (Mu f) f where
  ana = hylo embed

instance Functor f => Recursive (Nu f) f where
  cata = flip hylo project

-- FIXME: Relies on primitive recursion.
instance (Eq1 f, Functor f) => Eq (Nu f) where
  a == b = liftEq (==) (project a) (project b)

instance (Functor f, Show1 f) => Show (Nu f) where
  showsPrec = recursiveShowsPrec

instance Recursive [a] (XNor a) where
  cata = flip hylo project

instance Functor f => Recursive (Cofree f a) (EnvT a f) where
  cata = flip hylo project

instance Functor f => Corecursive (Cofree f a) (EnvT a f) where
  ana = hylo embed

instance Functor f => Recursive (Free f a) (FreeF f a) where
  cata = flip hylo project

instance Functor f => Corecursive (Free f a) (FreeF f a) where
  ana = hylo embed

-- TODO: If we can generalize these to an arbitrary '{Co}recursive t'
--       then they would no longer be unsafe.

distGFutu
  :: (Functor f, Functor h)
  => DistributiveLaw h f
  -> DistributiveLaw (Free h) f
distGFutu k = cata (\case
                   Pure a -> free . Pure <$> a
                   Free ft -> free . Free <$> k ft)

distGHisto
  :: (Functor f, Functor h)
  => DistributiveLaw f h
  -> DistributiveLaw f (Cofree h)
distGHisto k = ana $ uncurry EnvT . (fmap extract &&& k . fmap unwrap)
