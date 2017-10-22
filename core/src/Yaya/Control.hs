module Yaya.Control where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Env
import Control.Monad
import Data.Bitraversable
import Data.Distributive
import Data.Functor.Identity

import Yaya
import Data.Tuple

-- | Structures you can walk through step-by-step.
class Cursive t f | t -> f where
  embed :: Algebra f t
  project :: Coalgebra f t

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
cataM φ = cata $ φ <=< sequenceA

gcataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> GAlgebraM m w f a
  -> t
  -> m a
gcataM w φ = fmap extract . cataM (lowerAlgebraM w φ)

-- | A recursion scheme that allows to algebras to see each others’ results. (A
--   generalization of 'zygo'.)
mutu
  :: (Recursive t f, Functor f)
  => GAlgebra ((,) a) f b
  -> GAlgebra ((,) b) f a
  -> t
  -> a
mutu φ' φ = snd . cata (φ' . fmap swap &&& φ)

mutuM
  :: (Monad m, Recursive t f, Traversable f)
  => GAlgebraM m ((,) a) f b
  -> GAlgebraM m ((,) b) f a
  -> t
  -> m a
mutuM φ' φ = fmap snd . cataM (bisequence . (φ' . fmap swap &&& φ))

-- | This definition is different from the one given by 'gcataM $ distZygo φ''
--   because it has a monadic “helper” algebra.
zygoM
  :: (Monad m, Recursive t f, Traversable f)
  => AlgebraM m f b
  -> GAlgebraM m ((,) b) f a
  -> t
  -> m a
zygoM φ' φ =
  gcataM (distZygo (φ' <=< sequenceA))
         (φ <=< traverse (fmap swap . sequenceA . swap))

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

lambek :: (Cursive t f, Recursive t f, Functor f) => Coalgebra f t
lambek = cata $ fmap embed

colambek :: (Cursive t f, Corecursive t f, Functor f) => Algebra f t
colambek = ana $ fmap project

type DistributiveLaw f g = forall a. f (g a) -> g (f a)

distTraversable :: (Traversable f, Applicative g) => DistributiveLaw f g
distTraversable = sequenceA

distDistributive :: (Functor f, Distributive g) => DistributiveLaw f g
distDistributive = distribute

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
