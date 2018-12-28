{-# LANGUAGE GADTs #-}

module Yaya.Fold where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Env
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.Trans.Free
import Data.Bifunctor
import Data.Bitraversable
import Data.Distributive
import Data.Either.Combinators
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Day
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void
import Numeric.Natural

import Yaya.Fold.Common
import Yaya.Pattern

type Algebra f a = f a -> a
type GAlgebra w f a = f (w a) -> a
type ElgotAlgebra w f a = w (f a) -> a
type AlgebraM m f a = f a -> m a
type GAlgebraM m w f a = f (w a) -> m a
type ElgotAlgebraM m w f a = w (f a) -> m a

type Coalgebra f a = a -> f a
type GCoalgebra m f a = a -> f (m a)
type ElgotCoalgebra m f a = a -> m (f a)
type CoalgebraM m f a = a -> m (f a)
type GCoalgebraM m n f a = a -> m (f (n a))

-- | Structures you can walk through step-by-step.
class Steppable t f | t -> f where
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

-- | An implementation of `Eq` for any `Recursive` instance. Note that this is
--   actually more general than `Eq`, as it can compare between different
--   fixed-point representations of the same functor.
recursiveEq
  :: (Recursive t f, Steppable u f, Functor f, Foldable f, Eq1 f)
  => t -> u -> Bool
recursiveEq = cata2 equal

-- | An implementation of `Show` for any `Recursive` instance.
recursiveShowsPrec :: (Recursive t f, Show1 f) => Int -> t -> ShowS
recursiveShowsPrec prec =
  cata (showParen True . liftShowsPrec (const id) (foldMap id) prec)

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

instance Steppable [a] (XNor a) where
  embed Neither    = []
  embed (Both h t) = h : t
  project []      = Neither
  project (h : t) = Both h t

instance Steppable (NonEmpty a) (AndMaybe a) where
  embed (Only a)     = a :| []
  embed (Indeed a b) = a :| toList b
  project (a :| [])     = Only a
  project (a :| b : bs) = Indeed a (b :| bs)

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

-- | Combines two `Algebra`s with different carriers into a single tupled
--  `Algebra`.
zipAlgebras :: Functor f => Algebra f a -> Algebra f b -> Algebra f (a, b)
zipAlgebras f g = (f . fmap fst &&& g . fmap snd)

-- | Algebras over Day convolution are convenient for binary operations, but
--   aren’t directly handleable by `cata`.
lowerDay :: Steppable t g => Algebra (Day f g) a -> Algebra f (t -> a)
lowerDay φ fta t = φ (Day fta (project t) ($))

-- | By analogy with `liftA2` (which also relies on `Day`, at least
--   conceptually).
cata2 :: (Recursive t f, Steppable u g) => Algebra (Day f g) a -> t -> u -> a
cata2 = cata . lowerDay

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

gcataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> GAlgebraM m w f a
  -> t
  -> m a
gcataM w φ = fmap extract . cata (lowerAlgebraM w φ <=< sequenceA)

elgotCataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> ElgotAlgebraM m w f a
  -> t
  -> m a
elgotCataM w φ = φ <=< cata (fmap w . traverse (sequence . extend φ) <=< sequenceA)

ezygoM
  :: (Monad m, Recursive t f, Traversable f)
  => AlgebraM m f b
  -> ElgotAlgebraM m ((,) b) f a
  -> t
  -> m a
ezygoM φ' φ =
  fmap snd
  . cata ((\x@(b, _) -> (b,) <$> φ x)
          <=< bisequence . (φ' . fmap fst &&&  pure . fmap snd)
          <=< sequenceA)

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

lambek :: (Steppable t f, Recursive t f, Functor f) => Coalgebra f t
lambek = cata (fmap embed)

colambek :: (Steppable t f, Corecursive t f, Functor f) => Algebra f t
colambek = ana (fmap project)

-- | There are a number of distributive laws, including
--  `Data.Traversable.sequenceA`, `Data.Distributive.distribute`, and
--  `Data.Align.sequenceL`. Yaya also provides others for specific recursion
--   schemes.
type DistributiveLaw f g = forall a. f (g a) -> g (f a)

-- | A less-constrained `distribute` for `Identity`.
distIdentity :: Functor f => DistributiveLaw f Identity
distIdentity = Identity . fmap runIdentity

-- | A less-constrained `sequenceA` for `Identity`.
seqIdentity :: Functor f => DistributiveLaw Identity f
seqIdentity = fmap Identity . runIdentity

distTuple :: Functor f => Algebra f a -> DistributiveLaw f ((,) a)
distTuple φ = φ . fmap fst &&& fmap snd

distEnvT
  :: Functor f
  => Algebra f a
  -> DistributiveLaw f w
  -> DistributiveLaw f (EnvT a w)
distEnvT φ k = uncurry EnvT . (φ . fmap ask &&& k . fmap lowerEnvT)

seqEither :: Functor f => Coalgebra f a -> DistributiveLaw (Either a) f
seqEither ψ = fmap Left . ψ ||| fmap Right

-- | Converts an `Algebra` to one that annotates the tree with the result for
--   each node.
attributeAlgebra
  :: (Steppable t (EnvT a f), Functor f)
  => Algebra f a -> Algebra f t
attributeAlgebra φ ft = embed $ EnvT (φ (fmap (fst . runEnvT . project) ft)) ft

-- | Converts a `Coalgebra` to one that annotates the tree with the seed that
--   generated each node.
attributeCoalgebra :: Coalgebra f a -> Coalgebra (EnvT a f) a
attributeCoalgebra ψ = uncurry EnvT . (id &&& ψ)

-- | This is just a more obvious name for composing `lowerEnvT` with your
--   algebra directly.
ignoringAttribute :: Algebra f a -> Algebra (EnvT b f) a
ignoringAttribute φ = φ . lowerEnvT

unFree :: Steppable t f => Algebra (FreeF f t) t
unFree = \case
  Pure t  -> t
  Free ft -> embed ft

-- preservingAttribute :: (forall a. f a -> g a) -> EnvT a f b -> EnvT a g b
-- preservingAttribute = cohoist

-- instances for non-recursive types

constEmbed :: Algebra (Const a) a
constEmbed = getConst

constProject :: Coalgebra (Const a) a
constProject = Const

constCata :: Algebra (Const b) a -> b -> a
constCata φ = φ . Const

constAna :: Coalgebra (Const b) a -> a -> b
constAna ψ = getConst . ψ

instance Steppable (Either a b) (Const (Either a b)) where
  embed = constEmbed
  project = constProject

instance Recursive (Either a b) (Const (Either a b)) where
  cata = constCata

instance Corecursive (Either a b) (Const (Either a b)) where
  ana = constAna

instance Steppable (Maybe a) (Const (Maybe a)) where
  embed = constEmbed
  project = constProject

instance Recursive (Maybe a) (Const (Maybe a)) where
  cata = constCata

instance Corecursive (Maybe a) (Const (Maybe a)) where
  ana = constAna

-- | An endofunctor in the category of endofunctors.
class HFunctor h where
  hmap :: (forall a. f a -> g a) -> h f a -> h g a


-- | A functor from the category of endofunctors to *Hask*.
class DFunctor d where
  dmap :: (forall a. f a -> g a) -> d f -> d g

-- instance DFunctor Mu where
--   dmap f = cata (embed . f)

-- instance DFunctor Fix where
--   dmap f = ana (f . project)

-- instance DFunctor Nu where
--   dmap f = ana (f . project)

type BialgebraIso f a = Iso' (f a) a
type AlgebraPrism f a = Prism' (f a) a
type CoalgebraPrism f a = Prism' a (f a)

cursiveIso :: Steppable t f => BialgebraIso f t
cursiveIso = iso embed project

birecursiveIso
  :: (Recursive t f, Corecursive t f)
  => BialgebraIso f a
  -> Iso' t a
birecursiveIso alg = iso (cata (view alg)) (ana (review alg))

recursivePrism
  :: (Recursive t f, Corecursive t f, Traversable f)
  => AlgebraPrism f a
  -> Prism' t a
recursivePrism alg =
  prism
  (ana (review alg))
  (\t -> mapLeft (const t) $ cata (matching alg <=< sequenceA) t)
