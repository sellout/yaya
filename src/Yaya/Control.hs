module Yaya.Control where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Comonad.Env
import Control.Monad
import Data.Distributive
import Data.Functor.Identity

import Yaya

class Cursive t f | t -> f where
  embed :: Algebra f t
  project :: Coalgebra f t

class Cursive t f => Recursive t f where
  cata :: Algebra f a -> t -> a

class Cursive t f => Corecursive t f where
  ana :: Coalgebra f a -> a -> t

type Birecursive t f = (Recursive t f, Corecursive t f)

gcata
  :: (Recursive t f, Functor f, Comonad w)
  => DistributiveLaw f w
  -> GAlgebra w f a
  -> t
  -> a
gcata k φ = extract . cata (fmap φ . k . fmap duplicate)

cataM :: (Monad m, Recursive t f, Traversable f) => AlgebraM m f a -> t -> m a
cataM φ = cata $ φ <=< sequenceA

gcataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> GAlgebraM m w f a
  -> t
  -> m a
gcataM w φ = fmap extract . cataM (traverse φ . w . fmap duplicate)

lambek :: (Recursive t f, Functor f) => Coalgebra f t
lambek = cata $ fmap embed

colambek :: (Corecursive t f, Functor f) => Algebra f t
colambek = ana $ fmap project

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

type DistributiveLaw f g = forall a. f (g a) -> g (f a)

distTraversable :: (Traversable f, Applicative g) => DistributiveLaw f g
distTraversable = sequenceA

distDistributive :: (Functor f, Distributive g) => DistributiveLaw f g
distDistributive = distribute

distCata :: Functor f => DistributiveLaw f Identity
distCata = Identity . fmap runIdentity

distAna :: Functor f => DistributiveLaw Identity f
distAna = fmap Identity . runIdentity

distPara :: (Cursive t f, Functor f) => DistributiveLaw f ((,) t)
distPara = distZygo embed

distZygo :: Functor f => Algebra f a -> DistributiveLaw f ((,) a)
distZygo φ = φ . fmap fst &&& fmap snd

distZygoT
  :: (Functor f, Comonad w)
  => Algebra f a
  -> DistributiveLaw f w
  -> DistributiveLaw f (EnvT a w)
distZygoT φ k = uncurry EnvT . (φ . fmap ask &&& k . fmap lower)

distApo :: (Cursive t f, Functor f) => DistributiveLaw (Either t) f
distApo = distGApo project

distGApo :: Functor f => Coalgebra f a -> DistributiveLaw (Either a) f
distGApo ψ = fmap Left . ψ ||| fmap Right
