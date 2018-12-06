module Yaya.Control where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Comonad.Env
import Control.Monad
import Data.Bitraversable
import Data.Distributive
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Day
import Data.Functor.Identity

import Yaya

-- | Structures you can walk through step-by-step.
class Steppable t f | t -> f where
  embed :: Algebra f t
  project :: Coalgebra f t

-- | Inductive structures that can be reasoned about in the way we usually do –
--   with pattern matching.
class Recursive t f | t -> f where
  cata :: Algebra f a -> t -> a

-- | Algebras over Day convolution are convenient for binary operations, but
--   aren’t directly handleable by `cata`.
lowerDay :: Steppable t g => Algebra (Day f g) a -> Algebra f (t -> a)
lowerDay φ fta t = φ (Day fta (project t) ($))

-- | By analogy with `liftA2` (which also relies on `Day`, at least
--   conceptually).
cata2 :: (Recursive t f, Steppable u g) => Algebra (Day f g) a -> t -> u -> a
cata2 = cata . lowerDay

equal :: (Functor f, Foldable f, Eq1 f) => Algebra (Day f f) Bool
equal (Day f1 f2 fn) =
  liftEq (==) (void f1) (void f2)
  && and (zipWith fn (toList f1) (toList f2))

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

elgotCataM
  :: (Monad m, Recursive t f, Traversable f, Comonad w, Traversable w)
  => DistributiveLaw f w
  -> ElgotAlgebraM m w f a
  -> t
  -> m a
elgotCataM w φ = φ <=< cataM (fmap w . traverse (sequence . extend φ))

ezygoM
  :: (Monad m, Recursive t f, Traversable f)
  => AlgebraM m f b
  -> ElgotAlgebraM m ((,) b) f a
  -> t
  -> m a
ezygoM φ' φ =
  fmap snd
  . cataM ((\x@(b, _) -> (b,) <$> φ x)
           <=< bisequence . (φ' . fmap fst &&&  pure . fmap snd))

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
lambek = cata $ fmap embed

colambek :: (Steppable t f, Corecursive t f, Functor f) => Algebra f t
colambek = ana $ fmap project

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
  :: (Functor f, Comonad w)
  => Algebra f a
  -> DistributiveLaw f w
  -> DistributiveLaw f (EnvT a w)
distEnvT φ k = uncurry EnvT . (φ . fmap ask &&& k . fmap lower)

seqEither :: Functor f => Coalgebra f a -> DistributiveLaw (Either a) f
seqEither ψ = fmap Left . ψ ||| fmap Right

-- | Converts an `Algebra` to one that annotates the tree with the result for
--   each node.
attributeAlgebra
  :: (Steppable t (EnvT a f), Functor f)
  => Algebra f a -> Algebra f t
attributeAlgebra φ ft = embed $ EnvT (φ (fmap (fst . runEnvT . project) ft)) ft

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
