module Yaya.Control where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Env
import Control.Monad
import Data.Bitraversable
import Data.Distributive
import Data.Functor.Identity

import Yaya

class Embeddable t f | t -> f where
  embed :: Algebra f t

class Projectable t f | t -> f where
  project :: Coalgebra f t

-- | Structures you can walk through step-by-step.
type Steppable t f = (Embeddable t f, Projectable t f)

-- | Inductive structures that can be reasoned about in the way we usually do –
--   with pattern matching.
class Recursive t f | t -> f where
  cata :: Algebra f a -> t -> a

-- | Coinductive (potentially-infinite) structures that guarantee _productivity_
--   rather than termination.
class Corecursive t f | t -> f where
  ana :: Coalgebra f a -> a -> t

-- | Makes it possible to provide a 'GAlgebra' to 'cata'.
lowerAlgebra
  :: (Functor f, Comonad w)
  => DistributiveLaw f w
  -> GAlgebra w f a
  -> Algebra f (w a)
lowerAlgebra k φ = fmap φ . k . fmap duplicate

-- | Makes it possible to provide a 'GAlgebraM' to 'cataM'.
lowerAlgebraM
  :: (Applicative m, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> GAlgebraM m w f a
  -> AlgebraM m f (w a)
lowerAlgebraM k φ = traverse φ . k . fmap duplicate

-- | Makes it possible to provide a 'GCoalgebra' to 'ana'.
lowerCoalgebra
  :: (Functor f, Monad m)
  => DistributiveLaw m f
  -> GCoalgebra m f a
  -> Coalgebra f (m a)
lowerCoalgebra k ψ = fmap join . k . fmap ψ

-- | Makes it possible to provide a 'GCoalgebraM' to 'anaM'.
lowerCoalgebraM
  :: (Applicative m, Traversable f, Monad n, Traversable n)
  => DistributiveLaw n f
  -> GCoalgebraM m n f a
  -> CoalgebraM m f (n a)
lowerCoalgebraM k ψ = fmap (fmap join . k) . traverse ψ

gcata
  :: (Recursive t f, Functor f, Comonad w)
  => DistributiveLaw f w
  -> GAlgebra w f a
  -> t
  -> a
gcata k φ = extract . cata (lowerAlgebra k φ)

elgotCata
  :: (Recursive t f, Functor f, Comonad w)
  => DistributiveLaw f w
  -> ElgotAlgebra w f a
  -> t
  -> a
elgotCata k φ = φ . cata (k . fmap (extend φ))

cataM :: (Monad m, Recursive t f, Traversable f) => AlgebraM m f a -> t -> m a
cataM φ = cata (φ <=< sequenceA)

gcataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> GAlgebraM m w f a
  -> t
  -> m a
gcataM w φ = fmap extract . cataM (lowerAlgebraM w φ)

gana
  :: (Corecursive t f, Functor f, Monad m)
  => DistributiveLaw m f
  -> GCoalgebra m f a
  -> a
  -> t
gana k ψ = ana (lowerCoalgebra k ψ) . pure

elgotAna
  :: (Corecursive t f, Functor f, Monad m)
  => DistributiveLaw m f
  -> ElgotCoalgebra m f a
  -> a
  -> t
elgotAna k ψ = ana (fmap (>>= ψ) . k) . ψ

lambek :: (Embeddable t f, Recursive t f, Functor f) => Coalgebra f t
lambek = cata $ fmap embed

colambek :: (Projectable t f, Corecursive t f, Functor f) => Algebra f t
colambek = ana $ fmap project

-- | There are a number of distributive laws, including
--  `Data.Traversable.sequenceA`, `Data.Distributive.distribute`, and
--  `Data.Align.sequenceL`. Yaya also provides others for specific recursion
--   schemes.
type DistributiveLaw f g = forall a. f (g a) -> g (f a)

distCata :: Functor f => DistributiveLaw f Identity
distCata = Identity . fmap runIdentity

distAna :: Functor f => DistributiveLaw Identity f
distAna = fmap Identity . runIdentity

distZygo :: Functor f => Algebra f a -> DistributiveLaw f ((,) a)
distZygo φ = φ . fmap fst &&& fmap snd

distZygoT
  :: (Functor f, Comonad w)
  => Algebra f a
  -> DistributiveLaw f w
  -> DistributiveLaw f (EnvT a w)
distZygoT φ k = uncurry EnvT . (φ . fmap ask &&& k . fmap lower)

distGApo :: Functor f => Coalgebra f a -> DistributiveLaw (Either a) f
distGApo ψ = fmap Left . ψ ||| fmap Right

-- | Converts an `Algebra` to one that annotates the tree with the result for
--   each node.
attributeAlgebra
  :: (Steppable t (EnvT a f), Functor f)
  => Algebra f a -> Algebra f t
attributeAlgebra φ ft = embed $ EnvT (φ (fmap (fst . runEnvT . project) ft)) ft
