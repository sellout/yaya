{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}

module Yaya.Fold
  ( Algebra,
    AlgebraM,
    AlgebraPrism,
    BialgebraIso,
    Coalgebra,
    CoalgebraM,
    CoalgebraPrism,
    Corecursive (ana),
    DistributiveLaw,
    ElgotAlgebra,
    ElgotAlgebraM,
    ElgotCoalgebra,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    GCoalgebraM,
    Mu (Mu),
    Nu (Nu),
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    attributeAlgebra,
    attributeCoalgebra,
    birecursiveIso,
    cata2,
    colambek,
    constAna,
    constCata,
    constEmbed,
    constProject,
    distEnvT,
    distIdentity,
    distTuple,
    elgotAna,
    elgotCata,
    elgotCataM,
    ezygoM,
    gana,
    gcata,
    gcataM,
    ignoringAttribute,
    lambek,
    lowerAlgebra,
    lowerAlgebraM,
    lowerCoalgebra,
    lowerCoalgebraM,
    lowerDay,
    recursiveCompare,
    recursiveCompare',
    recursiveEq,
    recursiveEq',
    recursivePrism,
    recursiveShowsPrec,
    recursiveShowsPrec',
    seqEither,
    seqIdentity,
    steppableIso,
    steppableReadPrec,
    steppableReadPrec',
    unFree,
    zipAlgebraMs,
    zipAlgebras,
  )
where

import "base" Control.Applicative (Applicative (pure), (*>))
import "base" Control.Category (Category ((.)))
import "base" Control.Monad (Monad, join, (<=<), (=<<))
import "base" Data.Bifunctor (Bifunctor (bimap, first, second))
import "base" Data.Bitraversable (bisequence)
import "base" Data.Bool (Bool)
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable (toList))
import "base" Data.Function (const, flip, ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Functor.Classes
  ( Eq1 (liftEq),
    Ord1 (liftCompare),
    Read1 (liftReadPrec),
    Show1,
  )
import "base" Data.Int (Int)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord (compare, (<=)), Ordering)
import "base" Data.String (String)
import "base" Data.Traversable (sequenceA)
import "base" Data.Void (Void, absurd)
import "base" GHC.Read (expectP, list)
import "base" GHC.Show (appPrec1)
import "base" Numeric.Natural (Natural)
import "base" Text.Read
  ( Read (readListPrec, readPrec),
    ReadPrec,
    parens,
    prec,
    readListPrecDefault,
    step,
  )
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show (showsPrec), ShowS, showParen, showString)
import "comonad" Control.Comonad (Comonad (duplicate, extend, extract))
import "comonad" Control.Comonad.Trans.Env
  ( EnvT (EnvT),
    ask,
    lowerEnvT,
    runEnvT,
  )
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "free" Control.Monad.Trans.Free (Free, FreeF (Free, Pure), free, runFree)
import "kan-extensions" Data.Functor.Day (Day (Day))
import "lens" Control.Lens
  ( Const (Const, getConst),
    Identity (Identity, runIdentity),
    Iso',
    Prism',
    Traversable (traverse),
    iso,
    matching,
    prism,
    review,
    view,
  )
import "strict" Data.Strict.Classes (Strict (toStrict))
import "this" Yaya.Fold.Common
  ( compareDay,
    diagonal,
    equalDay,
    fromEither,
    showsPrecF,
  )
import "this" Yaya.Functor (DFunctor (dmap))
import "this" Yaya.Pattern
  ( AndMaybe (Indeed, Only),
    Either (Left, Right),
    Maybe (Just, Nothing),
    Pair ((:!:)),
    XNor (Both, Neither),
    fst,
    maybe,
    snd,
    uncurry,
  )
import "base" Prelude (Enum (pred, succ))

-- $setup
-- >>> :seti -XTypeApplications

type Algebra c f a = f a `c` a

type GAlgebra c w f a = f (w a) `c` a

type ElgotAlgebra c w f a = w (f a) `c` a

type AlgebraM c m f a = f a `c` m a

type GAlgebraM c m w f a = f (w a) `c` m a

type ElgotAlgebraM c m w f a = w (f a) `c` m a

type Coalgebra c f a = a `c` f a

type GCoalgebra c m f a = a `c` f (m a)

type ElgotCoalgebra c m f a = a `c` m (f a)

-- | Note that using a `CoalgebraM` “directly” is partial (e.g., with
--  `Yaya.Unsafe.Fold.anaM`). However, @ana . Compose@ can accept a `CoalgebraM`
--   and produce something like an effectful stream.
type CoalgebraM c m f a = a `c` m (f a)

type GCoalgebraM c m n f a = a `c` m (f (n a))

-- | This type class is lawless on its own, but there exist types that can’t
--   implement the corresponding `embed` operation. Laws are induced by
--   implementing either `Steppable` (which extends this) or `Corecursive`
--  (which doesn’t).
class Projectable c t f | t -> f where
  project :: Coalgebra c f t

-- | Structures you can walk through step-by-step.
class (Projectable c t f) => Steppable c t f | t -> f where
  embed :: Algebra c f t

-- | Inductive structures that can be reasoned about in the way we usually do –
--   with pattern matching.
class Recursive c t f | t -> f where
  cata :: Algebra c f a -> t `c` a

-- | Coinductive (potentially-infinite) structures that guarantee _productivity_
--   rather than termination.
class Corecursive c t f | t -> f where
  ana :: Coalgebra c f a -> a `c` t

-- | Like `recursiveEq`, but allows you to provide a custom comparator for @f@.
--
--   @since 0.6.1.0
recursiveEq' ::
  (Recursive (->) t f, Steppable (->) u f, Functor f, Foldable f) =>
  (f () -> f () -> Bool) ->
  t ->
  u ->
  Bool
recursiveEq' = cata2 . equalDay

-- | An implementation of `==` for any `Recursive` instance. Note that this is
--   actually more general than `Eq`’s `==`, as it can compare between different
--   fixed-point representations of the same functor.
--
--  __NB__: Use `recursiveEq'` if you need to use a custom comparator for @f@.
recursiveEq ::
  (Recursive (->) t f, Steppable (->) u f, Functor f, Foldable f, Eq1 f) =>
  t ->
  u ->
  Bool
recursiveEq = recursiveEq' $ liftEq (==)

-- | Like `recursiveCompare`, but allows you to provide a custom comparator for
--   @f@.
--
--   @since 0.6.1.0
recursiveCompare' ::
  (Recursive (->) t f, Steppable (->) u f, Functor f, Foldable f) =>
  (f () -> f () -> Ordering) ->
  t ->
  u ->
  Ordering
recursiveCompare' = cata2 . compareDay

-- | An implementation of `==` for any `Recursive` instance. Note that this is
--   actually more general than `Ord`’s `compare`, as it can compare between
--   different fixed-point representations of the same functor.
--
--  __NB__: Use `recursiveCompare'` if you need to use a custom comparator for
--          @f@.
--
--   @since 0.6.1.0
recursiveCompare ::
  (Recursive (->) t f, Steppable (->) u f, Functor f, Foldable f, Ord1 f) =>
  t ->
  u ->
  Ordering
recursiveCompare = recursiveCompare' $ liftCompare compare

embedOperation :: String
embedOperation = "embed"

-- | Like `recursiveShowsPrec`, but allows you to provide a custom display
--   function for @f@.
--
--   @since 0.6.1.0
recursiveShowsPrec' ::
  (Recursive (->) t f) => Algebra (->) f (Int -> ShowS) -> Int -> t -> ShowS
recursiveShowsPrec' showsFPrec = flip . cata $
  \f p ->
    showParen (appPrec1 <= p) $
      showString embedOperation . showString " " . showsFPrec f appPrec1

-- | An implementation of `showsPrec` for any `Recursive` instance.
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0) \
    && !MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
--
--  __FIXME__: There should be doctests here, but `doctest` crashes with these
--             tests in the very specific case of GHC 8.8.4 from Nixpkgs 23.11.
--             So, either figure out how to get them working there, or wait
--             until we no longer support that combination.
#else
--
-- >>> :{
--   recursiveShowsPrec
--     @(Mu (XNor String))
--     10
--     (embed (Both "a" (embed (Both "b" (embed Neither)))))
--     ""
-- :}
-- "embed (Both \"a\" (embed (Both \"b\" (embed Neither))))"
--
-- >>> :{
--   recursiveShowsPrec
--     @(Mu (XNor String))
--     11
--     (embed (Both "a" (embed (Both "b" (embed Neither)))))
--     ""
-- :}
-- "(embed (Both \"a\" (embed (Both \"b\" (embed Neither)))))"
#endif
--
--  __NB__: Use `recursiveShowsPrec'` if you need to use a custom serialization
--          function for @f@.
--
--  __NB__: This only requires `Recursive`, but the inverse operation is
--         `steppableReadPrec`, which requires `Steppable` instead.
recursiveShowsPrec :: (Recursive (->) t f, Show1 f) => Int -> t -> ShowS
recursiveShowsPrec = recursiveShowsPrec' $ flip showsPrecF

-- | Like `steppableReadPrec`, but allows you to provide a custom display
--   function for @f@.
--
--   @since 0.6.1.0
steppableReadPrec' ::
  (Steppable (->) t f) =>
  (ReadPrec t -> ReadPrec [t] -> ReadPrec (f t)) ->
  ReadPrec t
steppableReadPrec' readFPrec =
  let appPrec = 10
   in parens . prec appPrec . fmap embed $
        expectP (Lex.Ident embedOperation)
          *> step
            ( readFPrec (steppableReadPrec' readFPrec) . list $
                steppableReadPrec' readFPrec
            )

-- | An implementation of `readPrec` for any `Steppable` instance.
--
--  __NB__: Use `steppableReadPrec'` if you need to use a custom parsing
--          function  for @f@.
--
--  __NB__: This only requires `Steppable`, but the inverse operation is
--         `recursiveShowsPrec`, which requires `Recursive` instead.
--
--   @since 0.6.1.0
steppableReadPrec :: (Steppable (->) t f, Read1 f) => ReadPrec t
steppableReadPrec = steppableReadPrec' liftReadPrec

-- | A fixed-point operator for inductive / finite data structures.
--
--  *NB*: This is only guaranteed to be finite when @f a@ is strict in @a@
--       (having strict functors won't prevent `Nu` from being lazy). Using
--       @-XStrictData@ can help with this a lot.
newtype Mu f = Mu (forall a. Algebra (->) f a -> a)

instance (Functor f) => Projectable (->) (Mu f) f where
  project = lambek

instance (Functor f) => Steppable (->) (Mu f) f where
  embed m = Mu (\f -> f (fmap (cata f) m))

instance Recursive (->) (Mu f) f where
  cata φ (Mu f) = f φ

instance DFunctor Mu where
  dmap f (Mu run) = Mu (\φ -> run (φ . f))

instance (Functor f, Foldable f, Eq1 f) => Eq (Mu f) where
  (==) = recursiveEq

-- | @since 0.6.1.0
instance (Functor f, Foldable f, Ord1 f) => Ord (Mu f) where
  compare = recursiveCompare

-- | @since 0.6.1.0
instance (Functor f, Read1 f) => Read (Mu f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

instance (Show1 f) => Show (Mu f) where
  showsPrec = recursiveShowsPrec

-- | A fixed-point operator for coinductive / potentially-infinite data
--   structures.
data Nu f where Nu :: Coalgebra (->) f a -> a -> Nu f

instance (Functor f) => Projectable (->) (Nu f) f where
  project (Nu f a) = Nu f <$> f a

instance (Functor f) => Steppable (->) (Nu f) f where
  embed = colambek

instance Corecursive (->) (Nu f) f where
  ana = Nu

instance DFunctor Nu where
  dmap f (Nu φ a) = Nu (f . φ) a

-- | @since 0.6.1.0
instance (Functor f, Read1 f) => Read (Nu f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

instance Projectable (->) [a] (XNor a) where
  project [] = Neither
  project (h : t) = Both h t

instance Steppable (->) [a] (XNor a) where
  embed Neither = []
  embed (Both h t) = h : t

instance Projectable (->) (NonEmpty a) (AndMaybe a) where
  project (a :| []) = Only a
  project (a :| b : bs) = Indeed a (b :| bs)

instance Steppable (->) (NonEmpty a) (AndMaybe a) where
  embed (Only a) = a :| []
  embed (Indeed a b) = a :| toList b

instance Projectable (->) Natural Maybe where
  project 0 = Nothing
  project n = Just (pred n)

instance Steppable (->) Natural Maybe where
  embed = maybe 0 succ

instance Projectable (->) Void Identity where
  project = Identity

instance Steppable (->) Void Identity where
  embed = runIdentity

instance Recursive (->) Void Identity where
  cata _ = absurd

instance Projectable (->) (Cofree f a) (EnvT a f) where
  project (a :< ft) = EnvT a ft

instance Steppable (->) (Cofree f a) (EnvT a f) where
  embed (EnvT a ft) = a :< ft

instance Projectable (->) (Free f a) (FreeF f a) where
  project = runFree

instance Steppable (->) (Free f a) (FreeF f a) where
  embed = free

-- | Combines two `Algebra`s with different carriers into a single tupled
--  `Algebra`.
zipAlgebras ::
  (Functor f) =>
  Algebra (->) f a ->
  Algebra (->) f b ->
  Algebra (->) f (Pair a b)
zipAlgebras f g = bimap (f . fmap fst) (g . fmap snd) . diagonal

-- | Combines two `AlgebraM`s with different carriers into a single tupled
--  `AlgebraM`.
zipAlgebraMs ::
  (Applicative m, Functor f) =>
  AlgebraM (->) m f a ->
  AlgebraM (->) m f b ->
  AlgebraM (->) m f (Pair a b)
zipAlgebraMs f g = bisequence . bimap (f . fmap fst) (g . fmap snd) . diagonal

-- | Algebras over Day convolution are convenient for binary operations, but
--   aren’t directly handleable by `cata`.
lowerDay ::
  (Projectable (->) t g) => Algebra (->) (Day f g) a -> Algebra (->) f (t -> a)
lowerDay φ fta t = φ (Day fta (project t) ($))

-- | By analogy with `Control.Applicative.liftA2` (which also relies on `Day`,
--   at least conceptually).
cata2 ::
  (Recursive (->) t f, Projectable (->) u g) =>
  Algebra (->) (Day f g) a ->
  t ->
  u ->
  a
cata2 = cata . lowerDay

-- | Makes it possible to provide a `GAlgebra` to `cata`.
lowerAlgebra ::
  (Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  GAlgebra (->) w f a ->
  Algebra (->) f (w a)
lowerAlgebra k φ = fmap φ . k . fmap duplicate

-- | Makes it possible to provide a `GAlgebraM` to `Yaya.Zoo.cataM`.
lowerAlgebraM ::
  (Applicative m, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  GAlgebraM (->) m w f a ->
  AlgebraM (->) m f (w a)
lowerAlgebraM k φ = traverse φ . k . fmap duplicate

-- | Makes it possible to provide a `GCoalgebra` to `ana`.
lowerCoalgebra ::
  (Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  GCoalgebra (->) m f a ->
  Coalgebra (->) f (m a)
lowerCoalgebra k ψ = fmap join . k . fmap ψ

-- | Makes it possible to provide a `GCoalgebraM` to `Yaya.Unsafe.Fold.anaM`.
lowerCoalgebraM ::
  (Applicative m, Traversable f, Monad n, Traversable n) =>
  DistributiveLaw (->) n f ->
  GCoalgebraM (->) m n f a ->
  CoalgebraM (->) m f (n a)
lowerCoalgebraM k ψ = fmap (fmap join . k) . traverse ψ

gcata ::
  (Recursive (->) t f, Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  GAlgebra (->) w f a ->
  t ->
  a
gcata k φ = extract . cata (lowerAlgebra k φ)

elgotCata ::
  (Recursive (->) t f, Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  ElgotAlgebra (->) w f a ->
  t ->
  a
elgotCata k φ = φ . cata (k . fmap (extend φ))

gcataM ::
  (Monad m, Recursive (->) t f, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  GAlgebraM (->) m w f a ->
  t ->
  m a
gcataM w φ = fmap extract . cata (lowerAlgebraM w φ <=< sequenceA)

elgotCataM ::
  (Monad m, Recursive (->) t f, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  ElgotAlgebraM (->) m w f a ->
  t ->
  m a
elgotCataM w φ =
  φ <=< cata (fmap w . traverse (sequenceA . extend φ) <=< sequenceA)

ezygoM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f b ->
  ElgotAlgebraM (->) m (Pair b) f a ->
  t ->
  m a
ezygoM φ' φ =
  fmap snd
    . cata
      ( (\x@(b :!: _) -> (b :!:) <$> φ x)
          <=< bisequence . bimap (φ' . fmap fst) (pure . fmap snd) . diagonal
          <=< sequenceA
      )

gana ::
  (Corecursive (->) t f, Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  GCoalgebra (->) m f a ->
  a ->
  t
gana k ψ = ana (lowerCoalgebra k ψ) . pure

elgotAna ::
  (Corecursive (->) t f, Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  ElgotCoalgebra (->) m f a ->
  a ->
  t
elgotAna k ψ = ana (fmap (ψ =<<) . k) . ψ

lambek ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) => Coalgebra (->) f t
lambek = cata (fmap embed)

colambek ::
  (Projectable (->) t f, Corecursive (->) t f, Functor f) => Algebra (->) f t
colambek = ana (fmap project)

-- | There are a number of distributive laws, including
--  `sequenceA`, `Data.Distributive.distribute`, and `Data.Align.sequenceL`.
--   Yaya also provides others for specific recursion schemes.
type DistributiveLaw c f g = forall a. f (g a) `c` g (f a)

-- | A less-constrained `Data.Distributive.distribute` for `Identity`.
distIdentity :: (Functor f) => DistributiveLaw (->) f Identity
distIdentity = Identity . fmap runIdentity

-- | A less-constrained `sequenceA` for `Identity`.
seqIdentity :: (Functor f) => DistributiveLaw (->) Identity f
seqIdentity = fmap Identity . runIdentity

distTuple :: (Functor f) => Algebra (->) f a -> DistributiveLaw (->) f (Pair a)
distTuple φ = bimap (φ . fmap fst) (fmap snd) . diagonal

distEnvT ::
  (Functor f) =>
  Algebra (->) f a ->
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) f (EnvT a w)
distEnvT φ k =
  uncurry EnvT . bimap (φ . fmap ask) (k . fmap lowerEnvT) . diagonal

seqEither ::
  (Functor f) => Coalgebra (->) f a -> DistributiveLaw (->) (Either a) f
seqEither ψ = fromEither . bimap (fmap Left . ψ) (fmap Right)

-- | Converts an `Algebra` to one that annotates the tree with the result for
--   each node.
attributeAlgebra ::
  (Steppable (->) t (EnvT a f), Functor f) =>
  Algebra (->) f a ->
  Algebra (->) f t
attributeAlgebra φ ft =
  embed $ EnvT (φ (fmap (fst . toStrict . runEnvT . project) ft)) ft

-- | Converts a `Coalgebra` to one that annotates the tree with the seed that
--   generated each node.
attributeCoalgebra :: Coalgebra (->) f a -> Coalgebra (->) (EnvT a f) a
attributeCoalgebra ψ = uncurry EnvT . second ψ . diagonal

-- | This is just a more obvious name for composing `lowerEnvT` with your
--   algebra directly.
ignoringAttribute :: Algebra (->) f a -> Algebra (->) (EnvT b f) a
ignoringAttribute φ = φ . lowerEnvT

-- | It is somewhat common to have a natural transformation that looks like
--  @η :: forall a. f a -> Free g a@. This maps naturally to a `GCoalgebra` (to
--   pass to `Yaya.Zoo.apo`) with @η . project@, but the desired `Algebra` is
--   more likely to be @cata unFree . η@ than @embed . η@. See yaya-streams for
--   some examples of this.
unFree :: (Steppable (->) t f) => Algebra (->) (FreeF f t) t
unFree = \case
  Pure t -> t
  Free ft -> embed ft

-- preservingAttribute :: (forall a. f a -> g a) -> EnvT a f b -> EnvT a g b
-- preservingAttribute = cohoist

-- * instances for non-recursive types

constEmbed :: Algebra (->) (Const a) a
constEmbed = getConst

constProject :: Coalgebra (->) (Const a) a
constProject = Const

constCata :: Algebra (->) (Const b) a -> b -> a
constCata φ = φ . Const

constAna :: Coalgebra (->) (Const b) a -> a -> b
constAna ψ = getConst . ψ

instance Projectable (->) (Either a b) (Const (Either a b)) where
  project = constProject

instance Steppable (->) (Either a b) (Const (Either a b)) where
  embed = constEmbed

instance Recursive (->) (Either a b) (Const (Either a b)) where
  cata = constCata

instance Corecursive (->) (Either a b) (Const (Either a b)) where
  ana = constAna

instance Projectable (->) (Maybe a) (Const (Maybe a)) where
  project = constProject

instance Steppable (->) (Maybe a) (Const (Maybe a)) where
  embed = constEmbed

instance Recursive (->) (Maybe a) (Const (Maybe a)) where
  cata = constCata

instance Corecursive (->) (Maybe a) (Const (Maybe a)) where
  ana = constAna

-- * Optics

type BialgebraIso f a = Iso' (f a) a

type AlgebraPrism f a = Prism' (f a) a

type CoalgebraPrism f a = Prism' a (f a)

steppableIso :: (Steppable (->) t f) => BialgebraIso f t
steppableIso = iso embed project

birecursiveIso ::
  (Recursive (->) t f, Corecursive (->) t f) =>
  BialgebraIso f a ->
  Iso' t a
birecursiveIso alg = iso (cata (view alg)) (ana (review alg))

recursivePrism ::
  (Recursive (->) t f, Corecursive (->) t f, Traversable f) =>
  AlgebraPrism f a ->
  Prism' t a
recursivePrism alg =
  prism
    (ana (review alg))
    (\t -> first (const t) $ cata (matching alg <=< sequenceA) t)
