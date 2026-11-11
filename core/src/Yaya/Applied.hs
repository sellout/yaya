{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat #-}

-- __NB__: base-4.17 moves `IsList` to its own module, which avoids the unsafety
--         of importing "GHC.Exts". With prior versions of base, we at least
--         mark the module @Trustworthy@.
#if MIN_VERSION_base(4, 17, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Applied
  ( Void,
    absurd,
    append,
    at,
    atMay,
    constantly,
    drop',
    drop,
    fibonacci,
    fibonacciPolynomials,
    fromList,
    fromListN,
    fromMaybe,
    height,
    jacobsthal,
    length,
    lucas,
    lucasSequenceU,
    lucasSequenceV,
    maybeReify,
    mersenne,
    naturals,
    now,
    pell,
    reifyUpTo,
    reverse',
    reverse,
    runToEnd,
    succN,
    tail,
    take,
    takeUpTo,
    toList,
    truncate,
    vacuous,
    zeroN,
  )
where

import safe "base" Control.Category (id, (.))
import safe "base" Data.Foldable (Foldable, foldr)
import safe "base" Data.Function (flip)
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Functor.Identity (Identity, runIdentity)
import safe "base" Data.Int (Int)
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord, max)
import safe "base" Data.Semigroup (Semigroup, stimes, stimesMonoid, (<>))
import safe "base" Numeric.Natural (Natural)
import safe "free" Control.Monad.Trans.Free (FreeF (Free, Pure))
import safe "this" Yaya.Fold
  ( Algebra,
    Corecursive,
    Mu,
    Nu,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    cata2,
    embed,
    project,
    pattern Embed,
  )
import safe "this" Yaya.Fold.Common
  ( diagonal,
    fromEither,
    lucasSequence',
    maybeTakeNext,
    never,
    replaceNeither,
    takeAnother,
    takeAvailable,
    takeNext,
    toRight,
    truncate',
    unarySequence,
  )
import safe "this" Yaya.Fold.Native (Fix)
import safe "this" Yaya.Pattern
  ( Either (Left),
    Maybe (Just, Nothing),
    Pair,
    XNor (Both, Neither),
    maybe,
  )
import safe "base" Prelude (Integral, fromIntegral)

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList)
import qualified "base" GHC.IsList as IsList
#else
import "base" GHC.Exts (IsList)
import qualified "base" GHC.Exts as IsList
#endif

-- | Creates a computation that has already finished with the given result – the
--   unit of the partiality monad (see `Yaya.Zoo.Partial`).
--
-- @since 0.1.0
now :: (Steppable (->) t (Either a)) => a -> t
now = embed . Left

-- | This will collapse all the intermediate steps to get to the value that must
--   exist at the end.
--
-- @since 0.1.0
runToEnd :: (Recursive (->) t (Either a)) => t -> a
runToEnd = cata fromEither

-- | Converts exceptional divergence to non-termination.
--
-- @since 0.1.0
fromMaybe ::
  (Steppable (->) t (Either a), Corecursive (->) t (Either a)) => Maybe a -> t
fromMaybe = maybe (ana (toRight . never) ()) now

-- | The least fixed-point of `Identity`, which has no (total) inhabitants,
--   since there is no finite tower of `Identity` layers. This makes it an
--   encoding of `Data.Void.Void`.
--
-- @since 0.1.0
type Void = Mu Identity

-- | Eliminates a `Recursive` fixed-point of `Identity` (an uninhabited type)
--   into any type, as `Data.Void.absurd` does.
--
-- @since 0.1.0
absurd :: (Recursive (->) t Identity) => t -> a
absurd = cata runIdentity

-- | Lifts `absurd` through a `Functor`, as `Data.Void.vacuous` does.
--
-- @since 0.1.0
vacuous :: (Functor f, Recursive (->) t Identity) => f t -> f a
vacuous = fmap absurd

-- | Zero, as a `Steppable` fixed-point of `Maybe` (i.e., a Peano natural).
--
-- @since 0.1.0
zeroN :: (Steppable (->) t Maybe) => t
zeroN = embed Nothing

-- | The successor of a Peano natural.
--
-- @since 0.1.0
succN :: (Steppable (->) t Maybe) => t -> t
succN = embed . Just

-- | An algebra that computes the height of a node from the heights of its
--   children: zero for a node with no children, otherwise one more than the
--   greatest of theirs.
--
-- @since 0.1.0
height :: (Foldable f, Steppable (->) n Maybe, Ord n) => f n -> n
height = foldr (max . succN) zeroN

-- | The infinite stream of natural numbers, @0, 1, 2, …@.
--
-- @since 0.1.0
naturals :: (Steppable (->) n Maybe, Corecursive (->) t ((,) n)) => t
naturals = ana (unarySequence succN) zeroN

-- | The number of elements in a finite list.
--
-- @since 0.5.1
length :: (Recursive (->) t (XNor a), Steppable (->) n Maybe, Ord n) => t -> n
length = cata height

-- | Concatenates two lists.
--
-- @since 0.5.1
append :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u -> u
append front back = cata (embed . replaceNeither (project back)) front

-- | @since 0.5.1
instance Semigroup (Fix (XNor a)) where
  (<>) = append
  stimes = stimesMonoid

-- | @since 0.5.1
instance Monoid (Fix (XNor a)) where
  mempty = embed Neither

-- | @since 0.5.1
instance Semigroup (Mu (XNor a)) where
  (<>) = append
  stimes = stimesMonoid

-- | @since 0.5.1
instance Monoid (Mu (XNor a)) where
  mempty = embed Neither

-- | The algebra for dropping elements from a (co)list.
--
--  __NB__: This can’t be implemented using `Day`, because we need to know that
--          the `XNor` contains the fixed-point. And even if we could, it would
--          require `Steppable` rather than just `Projectable`.
--
-- @since 0.5.1
drop' :: (Projectable (->) t (XNor a)) => Maybe (t -> t) -> t -> t
drop' (Just fn) (Embed (Both _ t)) = fn t
drop' _ t = t

-- | Removes the first @n@ elements of a (co)list, or all of them if there are
--   fewer than @n@.
--
-- @since 0.5.1
drop :: (Recursive (->) n Maybe, Projectable (->) t (XNor a)) => n -> t -> t
drop = cata drop'

-- | Removes the first element of a (co)list, leaving an empty one unchanged.
--
-- @since 0.5.1
tail :: (Projectable (->) t (XNor a)) => t -> t
tail = drop (1 :: Natural)

-- | The algebra for `reverse`.
--
-- @since 0.5.1
reverse' ::
  (Steppable (->) t (XNor a)) =>
  XNor a (XNor a t -> XNor a t) ->
  XNor a t ->
  XNor a t
reverse' Neither = id
reverse' (Both a fn) = fn . Both a . embed

-- | Reverses a finite list.
--
-- @since 0.5.1
reverse :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u
reverse = embed . flip (cata reverse') Neither

-- | Extracts _no more than_ @n@ elements from the possibly-infinite sequence
--  @s@.
--
-- @since 0.1.0
takeUpTo ::
  ( Recursive (->) n Maybe,
    Projectable (->) s (XNor a),
    Steppable (->) l (XNor a)
  ) =>
  n ->
  s ->
  l
takeUpTo = cata2 (embed . takeAvailable)

-- | Extracts _exactly_ @n@ elements from the infinite stream @s@.
--
-- @since 0.1.0
take ::
  ( Recursive (->) n Maybe,
    Projectable (->) s ((,) a),
    Steppable (->) l (XNor a)
  ) =>
  n ->
  s ->
  l
take = cata2 (embed . takeAnother)

-- | Extracts the element at a finite index of an infinite sequence (a
--  `Data.List.!!` that can't fail).
--
-- @since 0.4.1
at :: (Recursive (->) n Maybe, Projectable (->) s ((,) a)) => n -> s -> a
at = cata2 takeNext

-- | Extracts the element at a finite index of a (co)list (a `Data.List.!!` that
--   fails with `Nothing`).
--
-- @since 0.4.1
atMay ::
  (Recursive (->) n Maybe, Projectable (->) s (XNor a)) => n -> s -> Maybe a
atMay = cata2 maybeTakeNext

-- | Turns part of a structure inductive, so it can be analyzed, without forcing
--   the entire tree.
--
-- @since 0.1.0
maybeReify ::
  (Projectable (->) s f, Steppable (->) l (FreeF f s), Functor f) =>
  Algebra (->) Maybe (s -> l)
maybeReify Nothing = embed . Pure
maybeReify (Just f) = embed . Free . fmap f . project

-- | Makes the first @n@ layers of a structure inductive (as `Free` layers),
--   leaving whatever is below them unexamined in `Pure`. This is `maybeReify`
--   iterated @n@ times.
--
-- @since 0.1.0
reifyUpTo ::
  ( Recursive (->) n Maybe,
    Projectable (->) s f,
    Steppable (->) l (FreeF f s),
    Functor f
  ) =>
  n ->
  s ->
  l
reifyUpTo = cata maybeReify

-- | The values of the Fibonacci polynomials at @x@,
--   @F_0(x), F_1(x), F_2(x), …@, i.e., the Lucas sequence @U_n(x, -1)@.
--
-- @since 0.1.0
fibonacciPolynomials :: (Integral i, Corecursive (->) t ((,) i)) => i -> t
fibonacciPolynomials x = lucasSequenceU x (-1)

-- | The Fibonacci numbers, @0, 1, 1, 2, 3, 5, …@, i.e., @F_n(1)@.
--
-- @since 0.1.0
fibonacci :: (Corecursive (->) t ((,) Int)) => t
fibonacci = fibonacciPolynomials 1

-- | The Lucas sequence @U_n(p, q)@, defined by @U_0 = 0@, @U_1 = 1@, and
--   @U_n = p U_(n-1) - q U_(n-2)@.
--
-- @since 0.1.0
lucasSequenceU :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceU p q = lucasSequence' p q `ana` (0, 1)

-- | The Lucas sequence @V_n(p, q)@, defined by @V_0 = 2@, @V_1 = p@, and
--   @V_n = p V_(n-1) - q V_(n-2)@.
--
-- @since 0.1.0
lucasSequenceV :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceV p q = lucasSequence' p q `ana` (2, p)

-- | The Lucas numbers, @2, 1, 3, 4, 7, …@, i.e., @V_n(1, -1)@.
--
-- @since 0.1.0
lucas :: (Integral i) => (Corecursive (->) t ((,) i)) => t
lucas = lucasSequenceV 1 (-1)

-- | The Pell numbers, @0, 1, 2, 5, 12, …@, i.e., @U_n(2, -1)@.
--
-- @since 0.1.0
pell :: (Integral i, Corecursive (->) t ((,) i)) => t
pell = lucasSequenceU 2 (-1)

-- | The Jacobsthal numbers, @0, 1, 1, 3, 5, 11, …@, i.e., @U_n(1, -2)@.
--
-- @since 0.1.0
jacobsthal :: (Integral i, Corecursive (->) t ((,) i)) => t
jacobsthal = lucasSequenceU 1 (-2)

-- | The Mersenne numbers, @2^n - 1@ (@0, 1, 3, 7, 15, …@), i.e., @U_n(3, 2)@.
--
-- @since 0.1.0
mersenne :: (Integral i, Corecursive (->) t ((,) i)) => t
mersenne = lucasSequenceU 3 2

-- | Creates an infinite stream of the provided value.
--
-- @since 0.1.0
constantly :: (Corecursive (->) t (Pair a)) => a -> t
constantly = ana diagonal

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  `Yaya.Applied.take`.
--
-- @since 0.1.0
truncate ::
  ( Recursive (->) n Maybe,
    Projectable (->) t f,
    Steppable (->) u (FreeF f ()),
    Functor f
  ) =>
  n ->
  t ->
  u
truncate = cata2 (embed . truncate')

-- | An implementation of `IsList.toList` for `Corecursive` fixed-points of
--  `XNor`.
--
-- @since 0.5.1
fromList :: (Corecursive (->) t (XNor a)) => [a] -> t
fromList = ana project

-- | An implementation of `IsList.fromListN` for `Steppable` fixed-points of
--  `XNor`.
--
--   This should return an empty structure if the `Int` is negative.
--
--   If the target structure isn’t `Steppable` or the target structure is
--  `Corecursive` (i.e., `Yaya.Unsafe.Fold.Applied.unsafeFromList` isn’t used),
--   then the default definition for `fromListN` should suffice.
--
-- @since 0.5.1
fromListN :: (Steppable (->) t (XNor a)) => Int -> [a] -> t
fromListN = cata2 (embed . takeAvailable) . fromIntegral @_ @Natural

-- | An implementation of `IsList.toList` for `Projectable` fixed-points of
--  `XNor`.
--
-- @since 0.5.1
toList :: (Projectable (->) t (XNor a)) => t -> [a]
toList = ana project

-- | This instance is safe, since both structures are lazy.
--
-- @since 0.5.1
instance IsList (Nu (XNor a)) where
  type Item (Nu (XNor a)) = a
  fromList = fromList
  toList = toList
