{-# LANGUAGE GADTs #-}

module Yaya.Data where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad.Trans.Free
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Void
import Numeric.Natural

import Yaya
import Yaya.Control

-- | A fixed-point operator for inductive / finite data structures.
data Mu f = Mu (forall a. Algebra f a -> a)

instance Functor f => Steppable (Mu f) f where
  embed m = Mu (\f -> f (fmap (cata f) m))
  project = lambek

instance Recursive (Mu f) f where
  cata φ (Mu f) = f φ

instance Show1 f => Show (Mu f) where
  showsPrec = recursiveShowsPrec

instance (Functor f, Foldable f, Eq1 f) => Eq (Mu f) where
  (==) = recursiveEq

-- | A fixed-point operator for coinductive / potentially-infinite data
--   structures.
data Nu f where Nu :: Coalgebra f a -> a -> Nu f

instance Functor f => Steppable (Nu f) f where
  embed = colambek
  project (Nu f a) = Nu f <$> f a

instance Corecursive (Nu f) f where
  ana = Nu

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = Neither | Both a b deriving (Functor, Foldable, Traversable)

instance Steppable [a] (XNor a) where
  embed Neither    = []
  embed (Both h t) = h : t
  project []      = Neither
  project (h : t) = Both h t

-- | Isomorphic to `(a, Maybe b)`, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only a | Indeed a b deriving (Functor, Foldable, Traversable)

instance Bifunctor AndMaybe where
  bimap f g = \case
    Only a -> Only (f a)
    Indeed a b -> Indeed (f a) (g b)

instance Steppable Natural Maybe where
  embed = maybe 0 succ
  project 0 = Nothing
  project n = Just (pred n)

-- TODO: This should at least move to the `Native` module.
instance Recursive Natural Maybe where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance Steppable Void Identity where
  embed = runIdentity
  project = Identity

instance Recursive Void Identity where
  cata _ = absurd

instance Steppable (Cofree f a) (EnvT a f) where
  embed (EnvT a ft) = a :< ft
  project (a :< ft) = EnvT a ft

instance Steppable (Free f a) (FreeF f a) where
  embed = free
  project = runFree
