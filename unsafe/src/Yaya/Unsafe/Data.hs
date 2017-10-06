{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Definitions and instances that use direct recursion.
module Yaya.Unsafe.Data where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Control.Monad.Trans.Free

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Unsafe.Control

data Fix f = Fix { unFix :: f (Fix f) }

instance Cursive (Fix f) f where
  embed = Fix
  project = unFix

instance Functor f => Recursive (Fix f) f where
  cata = flip hylo project

instance Functor f => Corecursive (Fix f) f where
  ana = hylo embed

instance Functor f => Corecursive (Mu f) f where
  ana = hylo embed

instance Functor f => Recursive (Nu f) f where
  cata = flip hylo project

instance Recursive [a] (XNor a) where
  cata = flip hylo project

instance Corecursive [a] (XNor a) where
  ana = hylo embed

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
