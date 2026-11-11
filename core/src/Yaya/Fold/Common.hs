{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
-- "Data.Functor.Day" is only inferred `Safe`.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
#endif

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   Common algebras that are useful when folding.
--
-- @since 0.1.0
module Yaya.Fold.Common
  ( binarySequence,
    definedOrInput,
    compareDay,
    diagonal,
    equal,
    equalDay,
    fromEither,
    height,
    le,
    lowerMonad,
    lowerMonoid,
    lowerSemigroup,
    lucasSequence',
    maybeTakeNext,
    never,
    replaceNeither,
    showsPrecF,
    size,
    takeAnother,
    takeAvailable,
    takeNext,
    ternarySequence,
    toRight,
    truncate',
    unarySequence,
    xnor,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category (id, (.))
import "base" Control.Monad (Monad, join)
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq ((==))
import "base" Data.Foldable (Foldable, and, fold, foldr, toList)
import "base" Data.Function (($), (&))
import "base" Data.Functor (Functor, fmap, void)
import "base" Data.Functor.Classes (Eq1, Show1, liftEq, liftShowsPrec)
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.Int (Int)
import "base" Data.List (zipWith)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ordering, max)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" GHC.Show (showList__)
import "base" Numeric.Natural (Natural)
import "base" Text.Show (ShowS)
import "free" Control.Monad.Trans.Free (FreeF (Free, Pure))
import "kan-extensions" Data.Functor.Day (Day (Day))
import "this" Yaya.Pattern
  ( AndMaybe,
    Either (Left, Right),
    Maybe (Just, Nothing),
    Pair ((:!:)),
    XNor (Both, Neither),
    andMaybe,
    either,
    maybe,
    xnor,
  )
import "base" Prelude (Integer, Num, (*), (+), (-))

-- | Converts the free monoid (a list) into some other `Monoid`.
--
-- @since 0.1.0
lowerMonoid :: (Monoid m) => (a -> m) -> XNor a m -> m
lowerMonoid = xnor mempty . ((<>) .)

-- | Converts the free semigroup (a non-empty list) into some other `Semigroup`.
--
-- @since 0.1.0
lowerSemigroup :: (Semigroup m) => (a -> m) -> AndMaybe a m -> m
lowerSemigroup f = andMaybe f ((<>) . f)

-- | Converts the free monad into some other `Monad`.
--
-- @since 0.1.0
lowerMonad :: (Monad m) => (forall x. f x -> m x) -> FreeF f a (m a) -> m a
lowerMonad f = \case
  Pure a -> pure a
  Free fm -> join (f fm)

-- | Provides equality over arbitrary pattern functors.
--
-- @since 0.6.1
equalDay ::
  (Functor f, Foldable f) => (f () -> f () -> Bool) -> Day f f Bool -> Bool
equalDay eqF (Day f1 f2 fn) =
  eqF (void f1) (void f2)
    && and (zipWith fn (toList f1) (toList f2))
{-# INLINEABLE equalDay #-}

-- | Provides equality over arbitrary pattern functors.
--
-- @since 0.1.0
equal :: (Functor f, Foldable f, Eq1 f) => Day f f Bool -> Bool
equal = equalDay $ liftEq (==)

-- | Provides ordering over arbitrary pattern functors.
--
-- @since 0.6.1
compareDay ::
  (Functor f, Foldable f) =>
  (f () -> f () -> Ordering) ->
  Day f f Ordering ->
  Ordering
compareDay compareF (Day f1 f2 fn) =
  compareF (void f1) (void f2)
    <> fold (zipWith fn (toList f1) (toList f2))
{-# INLINEABLE compareDay #-}

-- | Provides show over arbitrary pattern functors.
--
-- @since 0.6.1
showsPrecF :: (Show1 f) => Int -> f (Int -> ShowS) -> ShowS
showsPrecF = liftShowsPrec (&) (showList__ ($ 0))

-- TODO: Redefine this using `Natural`

-- | When folded, returns the height of the data structure.
--
-- @since 0.1.0
height :: (Foldable f) => f Integer -> Integer
height = (+ 1) . foldr max (-1)

-- NB: It seems like this could be some more general notion of this, like
--        size :: (Foldable f, Semiring a) => f a -> a
--        size = foldr (+) one

-- | When folded, returns the number of nodes in the data structure.
--
--  __NB__: This is /not/ the same as the length when applied to a list. I.e.,
--          @`Data.List.length` xs `+` 1 `==` `Yaya.Fold.cata` `size` xs@,
--          because this is counting the nodes of the structure (how many
--         `Neither`s and `Both`s), not how many elements (which would be
--          equivalent to only counting `Both`s).
--
-- @since 0.1.0
size :: (Foldable f) => f Natural -> Natural
size = foldr (+) 1

-- | Converts a provably infinite structure into a `Yaya.Zoo.Partial` one (that
--   will never terminate).
--
-- @since 0.1.0
toRight :: Identity b -> Either a b
toRight = Right . runIdentity

-- | Captures the input value if the application was undefined.
--
-- @since 0.4.1
definedOrInput :: (a -> Maybe b) -> a -> Either a b
definedOrInput f a = maybe (Left a) Right $ f a

-- | Collapses a `Yaya.Zoo.Partial` structure to a value (probably requiring
--   unsafe instances).
--
-- @since 0.1.0
fromEither :: Either a a -> a
fromEither = either id id

-- | Generates an infinite structure from an arbitrary seed.
--
-- @since 0.1.0
never :: a -> Identity a
never = Identity

-- | An algebra for `Yaya.Fold.cata2` that decides @m ≤ n@ for two Peano
--   naturals (fixed-points of `Maybe`).
--
-- @since 0.1.0
le :: Day Maybe Maybe Bool -> Bool
le = \case
  Day Nothing _ _ -> True
  Day (Just a) (Just b) f -> f a b
  Day (Just _) Nothing _ -> False

-- | Replaces a `Neither` layer with the first argument, leaving a `Both` layer
--   unchanged. This is the algebra behind `Yaya.Applied.append`.
--
-- @since 0.5.1
replaceNeither :: XNor a b -> XNor a b -> XNor a b
replaceNeither replacement = \case
  Neither -> replacement
  next -> next

-- | An algebra for `Yaya.Fold.cata2` that takes as many elements from a stream
--   as the natural number says: at zero it ends the list, otherwise it emits
--   the head of the stream and continues with its tail.
--
-- @since 0.1.0
takeAnother :: Day Maybe ((,) a) b -> XNor a b
takeAnother = \case
  Day Nothing _ _ -> Neither
  Day (Just x) (h, t) f -> Both h (f x t)

-- | Like `takeAnother`, but over a (co)list, so the result also ends if the
--   input does.
--
-- @since 0.1.0
takeAvailable :: Day Maybe (XNor a) b -> XNor a b
takeAvailable = \case
  Day Nothing _ _ -> Neither
  Day (Just x) t f -> fmap (f x) t

-- | An algebra for `Yaya.Fold.cata2` that indexes into a stream: at zero it
--   returns the head, otherwise it continues with the tail.
--
-- @since 0.4.1
takeNext :: Day Maybe ((,) a) a -> a
takeNext = \case
  Day Nothing (h, _) _ -> h
  Day (Just x) (_, t) f -> f x t

-- | Like `takeNext`, but over a (co)list, returning `Nothing` if it ends before
--   the index is reached.
--
-- @since 0.4.1
maybeTakeNext :: Day Maybe (XNor a) (Maybe a) -> Maybe a
maybeTakeNext = \case
  Day Nothing (Both h _) _ -> Just h
  Day (Just x) (Both _ t) f -> f x t
  Day _ Neither _ -> Nothing

-- | An algebra for `Yaya.Fold.cata2` that keeps one layer of the structure for
--   each step of the natural number, replacing everything below that depth with
--   @`Pure` ()@.
--
-- @since 0.1.0
truncate' :: (Functor f) => Day Maybe f a -> FreeF f () a
truncate' = \case
  Day Nothing _ _ -> Pure ()
  Day (Just n) fa f -> Free (fmap (f n) fa)

-- | Converts a single value into a tuple with the same value on both sides.
--   > x &&& y = (x *** y) . diagonal
--
-- @since 0.4.1
diagonal :: a -> Pair a a
diagonal x = x :!: x

-- * sequence generators

-- $sequence-generators
--   These functions are defined with different type parameters in order to
--   constrain the implementation, but to be used as coalgebras, all of the
--   parameters need to be specialized to the same type.

-- | Emits the seed and steps to its image under the function, so as a coalgebra
--   it unfolds @x@ to @x, f x, f (f x), …@.
--
-- @since 0.1.0
unarySequence :: (a -> b) -> a -> (a, b)
unarySequence f a = (a, f a)

-- | The seed is a window of the two most recent values; this emits the older
--   one and slides the window forward, applying the function to both to produce
--   the next.
--
-- @since 0.1.0
binarySequence :: (a -> b -> c) -> (a, b) -> (a, (b, c))
binarySequence f (a, b) = (a, (b, f a b))

-- | Like `binarySequence`, but with a window of three values.
--
-- @since 0.1.0
ternarySequence :: (a -> b -> c -> d) -> (a, b, c) -> (a, (b, c, d))
ternarySequence f (a, b, c) = (a, (b, c, f a b c))

-- | A coalgebra for the recurrence @x_n = p x_(n-1) - q x_(n-2)@. Seeded with
--   @(0, 1)@ it unfolds the Lucas sequence @U_n(p, q)@, and seeded with
--   @(2, p)@ it unfolds @V_n(p, q)@.
--
-- @since 0.1.0
lucasSequence' :: (Num n) => n -> n -> (n, n) -> (n, (n, n))
lucasSequence' p q = binarySequence (\n2 n1 -> p * n1 - q * n2)
