{-# LANGUAGE GADTs #-}

module Yaya.Data where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad.Trans.Free
import Numeric.Natural

import Yaya
import Yaya.Control

-- | A fixed-point operator for inductive / finite data structures.
data Mu f = Mu (forall a. Algebra f a -> a)

instance Functor f => Cursive (Mu f) f where
  embed m = Mu (\f -> f (fmap (cata f) m))
  project = lambek

instance Functor f => Recursive (Mu f) f where
  cata φ (Mu f) = f φ

-- | A fixed-point operator for coinductive / potentially-infinite data
--   structures.
data Nu f where Nu :: Coalgebra f a -> a -> Nu f

instance Functor f => Cursive (Nu f) f where
  embed = colambek
  project (Nu f a) = Nu f <$> f a

instance Functor f => Corecursive (Nu f) f where
  ana = Nu

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = None | Both a b deriving (Functor, Foldable, Traversable)

instance Cursive [a] (XNor a) where
  embed None = []
  embed (Both h t) = h : t
  project [] = None
  project (h : t) = Both h t

-- | Isomorphic to '(a, Maybe b)', it’s also the pattern functor for non-empty lists.
data AndMaybe a b = Only a | Indeed a b deriving (Functor, Foldable, Traversable)

instance Cursive Natural Maybe where
  embed = maybe 0 (+ 1)
  project 0 = Nothing
  project n = Just (n - 1)

instance Cursive (Cofree f a) (EnvT a f) where
  embed (EnvT a ft) = a :< ft
  project (a :< ft) = EnvT a ft

instance Cursive (Free f a) (FreeF f a) where
  embed = free
  project = runFree

-- instances for non-recursive types

constEmbed :: Algebra (Const a) a
constEmbed = getConst

constProject :: Coalgebra (Const a) a
constProject = Const

constCata :: Algebra (Const b) a -> b -> a
constCata φ = φ . Const

constAna :: Coalgebra (Const b) a -> a -> b
constAna ψ = getConst . ψ

instance Cursive (Either a b) (Const (Either a b)) where
  embed = constEmbed
  project = constProject

instance Recursive (Either a b) (Const (Either a b)) where
  cata = constCata

instance Corecursive (Either a b) (Const (Either a b)) where
  ana = constAna

instance Cursive (Maybe a) (Const (Maybe a)) where
  embed = constEmbed
  project = constProject

instance Recursive (Maybe a) (Const (Maybe a)) where
  cata = constCata

instance Corecursive (Maybe a) (Const (Maybe a)) where
  ana = constAna
