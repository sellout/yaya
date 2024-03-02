{-# LANGUAGE CPP #-}

-- __NB__: base-4.17 moves `IsList` to its own module, which avoids the unsafety
--         of importing "GHC.Exts". With prior versions of base, we at least
--         mark the module @Trustworthy@.
#if MIN_VERSION_base(4, 17, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Applied where

import safe "base" Control.Category (Category (id, (.)))
import safe "base" Data.Foldable (Foldable (foldr))
import safe "base" Data.Function (flip)
import safe "base" Data.Functor (Functor (fmap))
import safe "base" Data.Functor.Identity (Identity (runIdentity))
import safe "base" Data.Int (Int)
import safe "base" Data.Monoid (Monoid (mempty))
import safe "base" Data.Ord (Ord (max))
import safe "base" Data.Semigroup (Semigroup ((<>)))

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList)
import qualified "base" GHC.IsList as IsList
#else
import "base" GHC.Exts (IsList)
import qualified "base" GHC.Exts as IsList
#endif
import safe "base" Numeric.Natural (Natural)
import safe "free" Control.Monad.Trans.Free (FreeF (Free, Pure))
import safe "this" Yaya.Fold
  ( Algebra,
    Corecursive (ana),
    Mu,
    Nu,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    cata2,
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

now :: (Steppable (->) t (Either a)) => a -> t
now = embed . Left

-- | This will collapse all the intermediate steps to get to the value that must
--   exist at the end.
runToEnd :: (Recursive (->) t (Either a)) => t -> a
runToEnd = cata fromEither

-- | Converts exceptional divergence to non-termination.
fromMaybe :: (Steppable (->) t (Either a), Corecursive (->) t (Either a)) => Maybe a -> t
fromMaybe = maybe (ana (toRight . never) ()) now

type Void = Mu Identity

absurd :: (Recursive (->) t Identity) => t -> a
absurd = cata runIdentity

vacuous :: (Functor f, Recursive (->) t Identity) => f t -> f a
vacuous = fmap absurd

zeroN :: (Steppable (->) t Maybe) => t
zeroN = embed Nothing

succN :: (Steppable (->) t Maybe) => t -> t
succN = embed . Just

height :: (Foldable f, Steppable (->) n Maybe, Ord n) => f n -> n
height = foldr (max . succN) zeroN

naturals :: (Steppable (->) n Maybe, Corecursive (->) t ((,) n)) => t
naturals = ana (unarySequence succN) zeroN

length :: (Recursive (->) t (XNor a), Steppable (->) n Maybe, Ord n) => t -> n
length = cata height

append :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u -> u
append front back = cata (embed . replaceNeither (project back)) front

instance Semigroup (Fix (XNor a)) where
  (<>) = append

instance Monoid (Fix (XNor a)) where
  mempty = embed Neither

instance Semigroup (Mu (XNor a)) where
  (<>) = append

instance Monoid (Mu (XNor a)) where
  mempty = embed Neither

drop' :: (Projectable (->) t (XNor a)) => Maybe (t -> t) -> t -> t
drop' (Just fn) (project -> Both _ t) = fn t
drop' _ t = t

drop :: (Recursive (->) n Maybe, Projectable (->) t (XNor a)) => n -> t -> t
drop = cata drop'

tail :: (Projectable (->) t (XNor a)) => t -> t
tail = drop (1 :: Natural)

reverse' ::
  (Steppable (->) t (XNor a)) =>
  XNor a (XNor a t -> XNor a t) ->
  XNor a t ->
  XNor a t
reverse' Neither = id
reverse' (Both a fn) = fn . Both a . embed

reverse :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u
reverse = embed . flip (cata reverse') Neither

-- | Extracts _no more than_ @n@ elements from the possibly-infinite sequence
--  @s@.
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
at :: (Recursive (->) n Maybe, Projectable (->) s ((,) a)) => n -> s -> a
at = cata2 takeNext

-- | Extracts the element at a finite index of a (co)list (a `Data.List.!!` that
--   fails with `Nothing`).
atMay ::
  (Recursive (->) n Maybe, Projectable (->) s (XNor a)) => n -> s -> Maybe a
atMay = cata2 maybeTakeNext

-- | Turns part of a structure inductive, so it can be analyzed, without forcing
--   the entire tree.
maybeReify ::
  (Projectable (->) s f, Steppable (->) l (FreeF f s), Functor f) =>
  Algebra (->) Maybe (s -> l)
maybeReify Nothing = embed . Pure
maybeReify (Just f) = embed . Free . fmap f . project

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

fibonacciPolynomials :: (Integral i, Corecursive (->) t ((,) i)) => i -> t
fibonacciPolynomials x = lucasSequenceU x (-1)

fibonacci :: (Corecursive (->) t ((,) Int)) => t
fibonacci = fibonacciPolynomials 1

lucasSequenceU :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceU p q = lucasSequence' p q `ana` (0, 1)

lucasSequenceV :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceV p q = lucasSequence' p q `ana` (2, p)

lucas :: (Integral i) => (Corecursive (->) t ((,) i)) => t
lucas = lucasSequenceV 1 (-1)

pell :: (Integral i, Corecursive (->) t ((,) i)) => t
pell = lucasSequenceU 2 (-1)

jacobsthal :: (Integral i, Corecursive (->) t ((,) i)) => t
jacobsthal = lucasSequenceU 1 (-2)

mersenne :: (Integral i, Corecursive (->) t ((,) i)) => t
mersenne = lucasSequenceU 3 2

-- | Creates an infinite stream of the provided value.
constantly :: (Corecursive (->) t (Pair a)) => a -> t
constantly = ana diagonal

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  `Yaya.Applied.take`.
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
fromListN :: (Steppable (->) t (XNor a)) => Int -> [a] -> t
fromListN = cata2 (embed . takeAvailable) . fromIntegral @_ @Natural

-- | An implementation of `IsList.toList` for `Projectable` fixed-points of
--  `XNor`.
toList :: (Projectable (->) t (XNor a)) => t -> [a]
toList = ana project

-- | This instance is safe, since both structures are lazy.
instance IsList (Nu (XNor a)) where
  type Item (Nu (XNor a)) = a
  fromList = fromList
  toList = toList
