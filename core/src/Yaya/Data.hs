{-# LANGUAGE GADTs #-}

module Yaya.Data where

import Yaya
import Yaya.Control

data Mu f = Mu (forall a. Algebra f a -> a)

instance Functor f => Cursive (Mu f) f where
  embed m = Mu (\f -> f (fmap (cata f) m))
  project = lambek

instance Functor f => Recursive (Mu f) f where
  cata φ (Mu f) = f φ

data Nu f where Nu :: Coalgebra f a -> a -> Nu f

instance Functor f => Cursive (Nu f) f where
  embed = colambek
  project (Nu f a) = Nu f <$> f a

instance Functor f => Corecursive (Nu f) f where
  ana = Nu
