{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
-- "Data.Functor.Day" is only inferred `Safe`.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
#endif
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:steppableReadPrec' #-}

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Fold
  ( Algebra,
    AlgebraM,
    Coalgebra,
    CoalgebraM,
    Corecursive,
    DistributiveLaw,
    ElgotAlgebra,
    ElgotAlgebraM,
    ElgotCoalgebra,
    pattern Embed,
    pattern Project,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    GCoalgebraM,
    Mu,
    Nu,
    Projectable,
    Recursive,
    Steppable,
    ana,
    attributeAlgebra,
    attributeCoalgebra,
    cata,
    cata2,
    colambek,
    constAna,
    constCata,
    constEmbed,
    constProject,
    distCofreeT,
    distEnvT,
    distIdentity,
    distTuple,
    elgotAna,
    elgotCata,
    elgotCataM,
    embed,
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
    project,
    recursiveCompare,
    recursiveCompare',
    recursiveEq,
    recursiveEq',
    recursiveShowsPrec,
    recursiveShowsPrec',
    seqEither,
    seqFreeF,
    seqFreeT,
    seqIdentity,
    steppableReadPrec,
    steppableReadPrec',
    unFree,
    zipAlgebraMs,
    zipAlgebras,
  )
where

import "base" Control.Applicative (Applicative, pure, (*>))
import "base" Control.Category (id, (.))
import "base" Control.Monad (Monad, join, (<=<), (=<<))
import "base" Data.Bifunctor (bimap, second)
import "base" Data.Bitraversable (bisequence)
import "base" Data.Bool (Bool)
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable, toList)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (Functor, fmap, (<$>))
import "base" Data.Functor.Classes
  ( Eq1,
    Ord1,
    Read1,
    Show1,
    liftCompare,
    liftEq,
    liftReadPrec,
  )
import "base" Data.Functor.Const (Const (Const), getConst)
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.Int (Int)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord, Ordering, compare, (<=))
import "base" Data.String (String)
import "base" Data.Traversable (Traversable, sequenceA, traverse)
import "base" Data.Void (Void, absurd)
import "base" GHC.Read (expectP, list)
import "base" GHC.Show (appPrec1)
import "base" Numeric.Natural (Natural)
import "base" Text.Read
  ( Read,
    ReadPrec,
    parens,
    prec,
    readListPrec,
    readListPrecDefault,
    readPrec,
    step,
  )
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show, ShowS, showParen, showString, showsPrec)
import "comonad" Control.Comonad (Comonad, duplicate, extend, extract)
import "comonad" Control.Comonad.Trans.Env (EnvT (EnvT), lowerEnvT, runEnvT)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "free" Control.Monad.Trans.Free (Free, FreeF (Free, Pure), free, runFree)
import "kan-extensions" Data.Functor.Day (Day (Day))
import "strict" Data.Strict.Classes (toStrict)
import "this" Yaya.Fold.Common
  ( compareDay,
    diagonal,
    equalDay,
    fromEither,
    showsPrecF,
  )
import "this" Yaya.Functor (DFunctor, dmap)
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
import "base" Prelude (pred, succ)

-- NB: Prior to base 4.19, "Data.Functor" doesn’t export `unzip`, but starting
--     with base 4.22, the one from "Data.List.NonEmpty" is monomorphic.
#if MIN_VERSION_base(4, 19, 0)
import "base" Data.Functor (unzip)
#else
import "base" Data.List.NonEmpty (unzip)
#endif

-- $setup
-- >>> :seti -XTypeApplications

-- | An F-algebra in the category @c@ – a way to collapse one layer of @f@
--   into an @a@.
--
-- @since 0.1.0
type Algebra c f a = f a `c` a

-- | A generalized algebra, where each recursive position carries a comonadic
--   context @w@ alongside the folded result.
--
-- @since 0.1.0
type GAlgebra c w f a = f (w a) `c` a

-- | Like a `GAlgebra`, but with the comonadic context @w@ around the whole
--   layer rather than inside each recursive position.
--
-- @since 0.1.0
type ElgotAlgebra c w f a = w (f a) `c` a

-- | An `Algebra` with its result in a monad @m@ – that is, a Kleisli arrow out
--   of @f a@.
--
-- @since 0.1.0
type AlgebraM c m f a = f a `c` m a

-- | A `GAlgebra` with its result in a monad @m@.
--
-- @since 0.1.0
type GAlgebraM c m w f a = f (w a) `c` m a

-- | An `ElgotAlgebra` with its result in a monad @m@.
--
-- @since 0.1.0
type ElgotAlgebraM c m w f a = w (f a) `c` m a

-- | An F-coalgebra in the category @c@ – a way to expand a seed @a@ into one
--   layer of @f@, with new seeds in its recursive positions.
--
-- @since 0.1.0
type Coalgebra c f a = a `c` f a

-- | A generalized coalgebra, where each recursive position holds a seed in a
--   monadic context @m@.
--
-- @since 0.1.0
type GCoalgebra c m f a = a `c` f (m a)

-- | Like a `GCoalgebra`, but with the monad @m@ around the whole layer rather
--   than inside each recursive position.
--
-- @since 0.1.0
type ElgotCoalgebra c m f a = a `c` m (f a)

-- | Note that using a `CoalgebraM` “directly” is partial (e.g., with
--  `Yaya.Unsafe.Fold.anaM`). However, @ana . Compose@ can accept a `CoalgebraM`
--   and produce something like an effectful stream.
--
-- @since 0.1.0
type CoalgebraM c m f a = a `c` m (f a)

-- | A `GCoalgebra` with its result in a monad @m@.
--
-- @since 0.1.0
type GCoalgebraM c m n f a = a `c` m (f (n a))

-- | This type class is lawless on its own, but there exist types that can’t
--   implement the corresponding `embed` operation. Laws are induced by
--   implementing either `Steppable` (which extends this) or `Corecursive`
--  (which doesn’t).
--
-- @since 0.1.0
class Projectable c t f | t -> f where
  -- | Exposes the outermost layer of the structure – the @f@-coalgebra
  --   structure on @t@, dual to `embed`.
  --
  -- @since 0.1.0
  project :: Coalgebra c f t

-- | This pattern is useful when you need to match multiple levels of a pattern
--   functor. It won’t break exhaustiveness checking and, despite being named
--   `Embed`, only requires `Projectable`.
--
-- >>> case 3 :: Natural of {x@(Embed y) -> (x, y)}
-- (3,Just 2)
--
-- >>> case project [1, 2, 3] of {Both x (Embed (Both y z)) -> (x, y, z)}
-- (1,2,[3])
--
-- @since 1.0.0
pattern Embed :: (Projectable (->) t f) => f t -> t
pattern Embed ft <- (project -> ft)

-- | This pattern is useful when you need to match the projection of a structure
--   that has nicer embedded syntax, or if you’re only going to call `embed` on
--   the extracted values anyway. It won’t break exhaustiveness checking.
--
-- >>> case project (3 :: Natural) of {x@(Project y) -> (x, y)}
-- (Just 2,3)
--
-- >>> case project [1, 2, 3] of {x@(Project [1, 2, 3]) -> x}
-- Both 1 [2,3]
--
-- @since 1.0.0
pattern Project :: (Steppable (->) t f) => t -> f t
pattern Project t <- (embed -> t)

-- @COMPLETE@ on polymorphic pattern synonyms _requires_ a type constructor for
-- disambiguation before GHC 9.2. In that case, we define it on the few
-- constructors we have in scope here.
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
{-# COMPLETE Embed #-}
{-# COMPLETE Project #-}
#else
{-# COMPLETE Embed :: Mu #-}
{-# COMPLETE Embed :: Nu #-}
#endif

-- | Structures you can walk through step-by-step.
--
-- @since 0.1.0
class (Projectable c t f) => Steppable c t f | t -> f where
  -- | Wraps one layer of @f@ around its substructures – the @f@-algebra
  --   structure on @t@, dual to `project`.
  --
  -- @since 0.1.0
  embed :: Algebra c f t

-- | Inductive structures that can be reasoned about in the way we usually do –
--   with pattern matching.
--
-- @since 0.1.0
class Recursive c t f | t -> f where
  -- | The catamorphism, which folds the structure bottom-up with the given
  --   algebra.
  --
  -- @since 0.1.0
  cata :: Algebra c f a -> t `c` a

-- | Coinductive (potentially-infinite) structures that guarantee _productivity_
--   rather than termination.
--
-- @since 0.1.0
class Corecursive c t f | t -> f where
  -- | The anamorphism, which unfolds a seed top-down with the given coalgebra.
  --
  -- @since 0.1.0
  ana :: Coalgebra c f a -> a `c` t

-- | Like `recursiveEq`, but allows you to provide a custom comparator for @f@.
--
-- @since 0.6.1
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
--
-- @since 0.1.0
recursiveEq ::
  (Recursive (->) t f, Steppable (->) u f, Functor f, Foldable f, Eq1 f) =>
  t ->
  u ->
  Bool
recursiveEq = recursiveEq' $ liftEq (==)

-- | Like `recursiveCompare`, but allows you to provide a custom comparator for
--   @f@.
--
-- @since 0.6.1
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
-- @since 0.6.1
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
-- @since 0.6.1
recursiveShowsPrec' ::
  (Recursive (->) t f) => Algebra (->) f (Int -> ShowS) -> Int -> t -> ShowS
recursiveShowsPrec' showsFPrec = flip . cata $
  \f p ->
    showParen (appPrec1 <= p) $
      showString embedOperation . showString " " . showsFPrec f appPrec1
{-# INLINEABLE recursiveShowsPrec' #-}

-- | An implementation of `showsPrec` for any `Recursive` instance.
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
--
--  __NB__: Use `recursiveShowsPrec'` if you need to use a custom serialization
--          function for @f@.
--
--  __NB__: This only requires `Recursive`, but the inverse operation is
--         `steppableReadPrec`, which requires `Steppable` instead.
--
-- @since 0.1.0
recursiveShowsPrec :: (Recursive (->) t f, Show1 f) => Int -> t -> ShowS
recursiveShowsPrec = recursiveShowsPrec' $ flip showsPrecF
{-# INLINEABLE recursiveShowsPrec #-}

-- | Like `steppableReadPrec`, but allows you to provide a custom display
--   function for @f@.
--
-- @since 0.6.1
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
-- @since 0.6.1
steppableReadPrec :: (Steppable (->) t f, Read1 f) => ReadPrec t
steppableReadPrec = steppableReadPrec' liftReadPrec

-- | A fixed-point operator for inductive / finite data structures.
--
--  __NB__: This is only guaranteed to be finite when @f a@ is strict in @a@
--         (having strict functors won't prevent `Nu` from being lazy). Using
--          @-XStrictData@ can help with this a lot.
--
-- @since 0.1.0
newtype Mu f
  = -- | The Böhm–Berarducci (Church) encoding: a value /is/ its own fold,
    --   and `cata` simply applies it to the algebra.
    --
    -- @since 0.1.0
    Mu (forall a. Algebra (->) f a -> a)

type role Mu representational

-- | @since 0.1.0
instance (Functor f) => Projectable (->) (Mu f) f where
  project = lambek
  {-# INLINEABLE project #-}

-- | @since 0.1.0
instance (Functor f) => Steppable (->) (Mu f) f where
  embed m = Mu (\f -> f (fmap (cata f) m))

-- | @since 0.1.0
instance Recursive (->) (Mu f) f where
  cata φ (Mu f) = f φ

-- | @since 0.2.0
instance DFunctor Mu where
  dmap f (Mu run) = Mu (\φ -> run (φ . f))

-- | @since 0.1.0
instance (Functor f, Foldable f, Eq1 f) => Eq (Mu f) where
  (==) = recursiveEq
  {-# INLINEABLE (==) #-}

-- | @since 0.6.1
instance (Functor f, Foldable f, Ord1 f) => Ord (Mu f) where
  compare = recursiveCompare

-- | @since 0.6.1
instance (Functor f, Read1 f) => Read (Mu f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

-- | @since 0.1.0
instance (Show1 f) => Show (Mu f) where
  showsPrec = recursiveShowsPrec

-- | A fixed-point operator for coinductive / potentially-infinite data
--   structures.
--
-- @since 0.1.0
data Nu f where
  -- | A suspended unfold: a `Coalgebra` paired with a seed. `ana` is this
  --   constructor.
  --
  -- @since 0.1.0
  Nu :: Coalgebra (->) f a -> a -> Nu f

type role Nu representational

-- | @since 0.1.0
instance (Functor f) => Projectable (->) (Nu f) f where
  project (Nu f a) = Nu f <$> f a
  {-# INLINEABLE project #-}

-- | @since 0.1.0
instance (Functor f) => Steppable (->) (Nu f) f where
  embed = colambek
  {-# INLINEABLE embed #-}

-- | @since 0.1.0
instance Corecursive (->) (Nu f) f where
  ana = Nu

-- | @since 0.2.0
instance DFunctor Nu where
  dmap f (Nu φ a) = Nu (f . φ) a

-- | @since 0.6.1
instance (Functor f, Read1 f) => Read (Nu f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

-- | @since 0.1.0
instance Projectable (->) [a] (XNor a) where
  project [] = Neither
  project (h : t) = Both h t

-- | @since 0.1.0
instance Steppable (->) [a] (XNor a) where
  embed Neither = []
  embed (Both h t) = h : t

-- | @since 0.1.0
instance Projectable (->) (NonEmpty a) (AndMaybe a) where
  project (a :| []) = Only a
  project (a :| b : bs) = Indeed a (b :| bs)

-- | @since 0.1.0
instance Steppable (->) (NonEmpty a) (AndMaybe a) where
  embed (Only a) = a :| []
  embed (Indeed a b) = a :| toList b

-- | @since 0.1.0
instance Projectable (->) Natural Maybe where
  project 0 = Nothing
  project n = Just (pred n)

-- | @since 0.1.0
instance Steppable (->) Natural Maybe where
  embed = maybe 0 succ

-- | @since 0.1.0
instance Projectable (->) Void Identity where
  project = Identity

-- | @since 0.1.0
instance Steppable (->) Void Identity where
  embed = runIdentity

-- | @since 0.1.0
instance Recursive (->) Void Identity where
  cata _ = absurd

-- | @since 0.1.0
instance Projectable (->) (Cofree f a) (EnvT a f) where
  project (a :< ft) = EnvT a ft

-- | @since 0.1.0
instance Steppable (->) (Cofree f a) (EnvT a f) where
  embed (EnvT a ft) = a :< ft

-- | @since 0.1.0
instance Projectable (->) (Free f a) (FreeF f a) where
  project = runFree

-- | @since 0.1.0
instance Steppable (->) (Free f a) (FreeF f a) where
  embed = free

-- | Combines two `Algebra`s with different carriers into a single tupled
--  `Algebra`.
--
-- @since 0.1.0
zipAlgebras ::
  (Functor f) =>
  Algebra (->) f a ->
  Algebra (->) f b ->
  Algebra (->) f (Pair a b)
zipAlgebras f g = bimap (f . fmap fst) (g . fmap snd) . diagonal

-- | Combines two `AlgebraM`s with different carriers into a single tupled
--  `AlgebraM`.
--
-- @since 0.3.2
zipAlgebraMs ::
  (Applicative m, Functor f) =>
  AlgebraM (->) m f a ->
  AlgebraM (->) m f b ->
  AlgebraM (->) m f (Pair a b)
zipAlgebraMs f g = bisequence . bimap (f . fmap fst) (g . fmap snd) . diagonal

-- | Algebras over Day convolution are convenient for binary operations, but
--   aren’t directly handleable by `cata`.
--
-- @since 0.1.0
lowerDay ::
  (Projectable (->) t g) => Algebra (->) (Day f g) a -> Algebra (->) f (t -> a)
lowerDay φ fta t = φ (Day fta (project t) ($))

-- | By analogy with `Control.Applicative.liftA2` (which also relies on `Day`,
--   at least conceptually).
--
-- @since 0.1.0
cata2 ::
  (Recursive (->) t f, Projectable (->) u g) =>
  Algebra (->) (Day f g) a ->
  t ->
  u ->
  a
cata2 = cata . lowerDay

-- | Makes it possible to provide a `GAlgebra` to `cata`.
--
-- @since 0.1.0
lowerAlgebra ::
  (Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  GAlgebra (->) w f a ->
  Algebra (->) f (w a)
lowerAlgebra k φ = fmap φ . k . fmap duplicate

-- | Makes it possible to provide a `GAlgebraM` to `Yaya.Zoo.cataM`.
--
-- @since 0.1.0
lowerAlgebraM ::
  (Applicative m, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  GAlgebraM (->) m w f a ->
  AlgebraM (->) m f (w a)
lowerAlgebraM k φ = traverse φ . k . fmap duplicate

-- | Makes it possible to provide a `GCoalgebra` to `ana`.
--
-- @since 0.1.0
lowerCoalgebra ::
  (Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  GCoalgebra (->) m f a ->
  Coalgebra (->) f (m a)
lowerCoalgebra k ψ = fmap join . k . fmap ψ

-- | Makes it possible to provide a `GCoalgebraM` to `Yaya.Unsafe.Fold.anaM`.
--
-- @since 0.1.0
lowerCoalgebraM ::
  (Applicative m, Traversable f, Monad n, Traversable n) =>
  DistributiveLaw (->) n f ->
  GCoalgebraM (->) m n f a ->
  CoalgebraM (->) m f (n a)
lowerCoalgebraM k ψ = fmap (fmap join . k) . traverse ψ

-- | A generalized catamorphism, folding with a `GAlgebra` by using the
--   `DistributiveLaw` to thread the comonad @w@ through each layer.
--
-- @since 0.1.0
gcata ::
  (Recursive (->) t f, Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  GAlgebra (->) w f a ->
  t ->
  a
gcata k φ = extract . cata (lowerAlgebra k φ)

-- | Folds with an `ElgotAlgebra`, using the `DistributiveLaw` to thread the
--   comonad @w@ through each layer.
--
-- @since 0.1.0
elgotCata ::
  (Recursive (->) t f, Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  ElgotAlgebra (->) w f a ->
  t ->
  a
elgotCata k φ = φ . cata (k . fmap (extend φ))

-- | A monadic `gcata`, sequencing the effects of the substructures (in
--   `traverse` order) before those of each node.
--
-- @since 0.1.0
gcataM ::
  (Monad m, Recursive (->) t f, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  GAlgebraM (->) m w f a ->
  t ->
  m a
gcataM w φ = fmap extract . cata (lowerAlgebraM w φ <=< sequenceA)

-- | A monadic `elgotCata`, sequencing the effects of the substructures (in
--   `traverse` order) before those of each node.
--
-- @since 0.1.0
elgotCataM ::
  (Monad m, Recursive (->) t f, Traversable f, Comonad w, Traversable w) =>
  DistributiveLaw (->) f w ->
  ElgotAlgebraM (->) m w f a ->
  t ->
  m a
elgotCataM w φ =
  φ <=< cata (fmap w . traverse (sequenceA . extend φ) <=< sequenceA)

-- | A monadic zygomorphism with an Elgot-style main algebra: at each node, the
--   helper `AlgebraM` runs first, and the main algebra receives its result
--   paired with the layer of results from the substructures.
--
-- @since 0.1.0
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

-- | A generalized anamorphism, unfolding with a `GCoalgebra` by using the
--   `DistributiveLaw` to thread the monad @m@ through each layer.
--
-- @since 0.1.0
gana ::
  (Corecursive (->) t f, Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  GCoalgebra (->) m f a ->
  a ->
  t
gana k ψ = ana (lowerCoalgebra k ψ) . pure

-- | Unfolds with an `ElgotCoalgebra`, using the `DistributiveLaw` to thread
--   the monad @m@ through each layer.
--
-- @since 0.1.0
elgotAna ::
  (Corecursive (->) t f, Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  ElgotCoalgebra (->) m f a ->
  a ->
  t
elgotAna k ψ = ana (fmap (ψ =<<) . k) . ψ

-- | Lambek’s lemma: the structure map of an initial algebra is an
--   isomorphism, and @`cata` (fmap `embed`)@ is the inverse of `embed`. This
--   gives a `project` for any `Recursive` `Steppable` type.
--
-- @since 0.1.0
lambek ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) => Coalgebra (->) f t
lambek = cata (fmap embed)

-- | The dual of Lambek’s lemma: the structure map of a terminal coalgebra is
--   an isomorphism, and @`ana` (fmap `project`)@ is the inverse of `project`.
--   This gives an `embed` for any `Corecursive` `Projectable` type.
--
-- @since 0.1.0
colambek ::
  (Projectable (->) t f, Corecursive (->) t f, Functor f) => Algebra (->) f t
colambek = ana (fmap project)

-- | There are a number of distributive laws, including
--  `sequenceA`, `Data.Distributive.distribute`, and `Data.Align.sequenceL`.
--   Yaya also provides others for specific recursion schemes.
--
-- @since 0.1.0
type DistributiveLaw c f g = forall a. f (g a) `c` g (f a)

-- | A less-constrained `Data.Distributive.distribute` for `Identity`.
--
-- @since 0.1.0
distIdentity :: (Functor f) => DistributiveLaw (->) f Identity
distIdentity = Identity . fmap runIdentity

-- | A less-constrained `sequenceA` for `Identity`.
--
-- @since 0.1.0
seqIdentity :: (Functor f) => DistributiveLaw (->) Identity f
seqIdentity = fmap Identity . runIdentity

-- | The `DistributiveLaw` behind `Yaya.Zoo.zygo`, which runs the helper
--   `Algebra` over the first components to produce the new annotation.
--
-- @since 0.1.0
distTuple :: (Functor f) => Algebra (->) f a -> DistributiveLaw (->) f (Pair a)
distTuple φ = bimap (φ . fmap fst) (fmap snd) . diagonal

-- | Distributes @f@ over @`EnvT` a w@, combining the annotations with the
--   given function and distributing over @w@ with the given `DistributiveLaw`.
--
-- @since 0.1.0
distEnvT ::
  (Functor f) =>
  -- |
  --
  --  __NB__: To be a true `DistributiveLaw`, this must be an @`Algebra` (->) f
  --          a@, but this more general version is useful in certain cases (like
  --         `distCofreeT`).
  (f a -> b) ->
  DistributiveLaw (->) f w ->
  f (EnvT a w c) ->
  EnvT b w (f c)
distEnvT φ k = uncurry EnvT . bimap φ k . toStrict . unzip . fmap runEnvT

-- | Lifts a `DistributiveLaw` of @f@ over @h@ to one of @f@ over a cofree
--   comonad on @h@ (any @t@ whose pattern functor is @`EnvT` a h@). With
--   @h ~ f@ and `id`, this is the law behind `Yaya.Zoo.histo`.
--
-- @since 1.0.0
distCofreeT ::
  ( Corecursive (->) (t (f a)) (EnvT (f a) h),
    Projectable (->) (t a) (EnvT a h),
    Functor f
  ) =>
  DistributiveLaw (->) f h ->
  f (t a) ->
  t (f a)
distCofreeT k = ana $ distEnvT id k . fmap project

-- | Distributes @`FreeF` m a@ over @f@, handling `Pure` with the given
--   function and `Free` with the given `DistributiveLaw`. Dual to `distEnvT`.
--
-- @since 1.0.0
seqFreeF ::
  (Functor f) =>
  -- |
  --
  --  __NB__: To be a true `DistributiveLaw`, this must be a @`Coalgebra` (->) f
  --          a@, but this more general version is useful in certain cases (like
  --         `seqFreeT`).
  (a -> f b) ->
  DistributiveLaw (->) m f ->
  FreeF m a (f c) ->
  f (FreeF m b c)
seqFreeF ψ k = \case
  Pure a -> Pure <$> ψ a
  Free ft -> Free <$> k ft

-- | Lifts a `DistributiveLaw` of @h@ over @f@ to one of a free monad on @h@
--   (any @t@ whose pattern functor is @`FreeF` h a@) over @f@. With @h ~ f@ and
--   `id`, this is the law behind `Yaya.Zoo.futu`. Dual to `distCofreeT`.
--
-- @since 1.0.0
seqFreeT ::
  ( Recursive (->) (t (f a)) (FreeF h (f a)),
    Steppable (->) (t a) (FreeF h a),
    Functor f
  ) =>
  DistributiveLaw (->) h f ->
  t (f a) ->
  f (t a)
seqFreeT k = cata $ fmap embed . seqFreeF id k

-- | The `DistributiveLaw` behind `Yaya.Zoo.gapo`, which unfolds `Left` values
--   with the given `Coalgebra` and leaves `Right` seeds in place. Dual to
--   `distTuple`.
--
-- @since 0.1.0
seqEither ::
  (Functor f) => Coalgebra (->) f a -> DistributiveLaw (->) (Either a) f
seqEither ψ = fromEither . bimap (fmap Left . ψ) (fmap Right)

-- | Converts an `Algebra` to one that annotates the tree with the result for
--   each node.
--
-- @since 0.1.0
attributeAlgebra ::
  (Steppable (->) t (EnvT a f), Functor f) =>
  Algebra (->) f a ->
  Algebra (->) f t
attributeAlgebra φ ft =
  embed $ EnvT (φ (fmap (fst . toStrict . runEnvT . project) ft)) ft

-- | Converts a `Coalgebra` to one that annotates the tree with the seed that
--   generated each node.
--
-- @since 0.1.0
attributeCoalgebra :: Coalgebra (->) f a -> Coalgebra (->) (EnvT a f) a
attributeCoalgebra ψ = uncurry EnvT . second ψ . diagonal

-- | This is just a more obvious name for composing `lowerEnvT` with your
--   algebra directly.
--
-- @since 0.1.0
ignoringAttribute :: Algebra (->) f a -> Algebra (->) (EnvT b f) a
ignoringAttribute φ = φ . lowerEnvT

-- | It is somewhat common to have a natural transformation that looks like
--  @η :: forall a. f a -> Free g a@. This maps naturally to a `GCoalgebra` (to
--   pass to `Yaya.Zoo.apo`) with @η . project@, but the desired `Algebra` is
--   more likely to be @cata unFree . η@ than @embed . η@. See yaya-streams for
--   some examples of this.
--
-- @since 0.1.0
unFree :: (Steppable (->) t f) => Algebra (->) (FreeF f t) t
unFree = \case
  Pure t -> t
  Free ft -> embed ft

-- preservingAttribute :: (forall a. f a -> g a) -> EnvT a f b -> EnvT a g b
-- preservingAttribute = cohoist

-- * instances for non-recursive types

-- | `embed` for any type viewed as the fixed point of its constant functor.
--
-- @since 0.1.0
constEmbed :: Algebra (->) (Const a) a
constEmbed = getConst

-- | `project` for any type viewed as the fixed point of its constant functor.
--
-- @since 0.1.0
constProject :: Coalgebra (->) (Const a) a
constProject = Const

-- | `cata` for any type viewed as the fixed point of its constant functor –
--   the algebra is applied exactly once.
--
-- @since 0.1.0
constCata :: Algebra (->) (Const b) a -> b -> a
constCata φ = φ . Const

-- | `ana` for any type viewed as the fixed point of its constant functor –
--   the coalgebra is applied exactly once.
--
-- @since 0.1.0
constAna :: Coalgebra (->) (Const b) a -> a -> b
constAna ψ = getConst . ψ

-- | @since 0.1.0
instance Projectable (->) (Either a b) (Const (Either a b)) where
  project = constProject

-- | @since 0.1.0
instance Steppable (->) (Either a b) (Const (Either a b)) where
  embed = constEmbed

-- | @since 0.1.0
instance Recursive (->) (Either a b) (Const (Either a b)) where
  cata = constCata

-- | @since 0.1.0
instance Corecursive (->) (Either a b) (Const (Either a b)) where
  ana = constAna

-- | @since 0.1.0
instance Projectable (->) (Maybe a) (Const (Maybe a)) where
  project = constProject

-- | @since 0.1.0
instance Steppable (->) (Maybe a) (Const (Maybe a)) where
  embed = constEmbed

-- | @since 0.1.0
instance Recursive (->) (Maybe a) (Const (Maybe a)) where
  cata = constCata

-- | @since 0.1.0
instance Corecursive (->) (Maybe a) (Const (Maybe a)) where
  ana = constAna
