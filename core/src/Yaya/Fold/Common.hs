-- | Common algebras that are useful when folding.
module Yaya.Fold.Common where

import "base" Control.Applicative (Applicative (..))
import "base" Control.Category (Category (..))
import "base" Control.Monad (Monad, join)
import "free" Control.Monad.Trans.Free (FreeF (..))
import "base" Data.Bool (Bool (..), (&&))
import "base" Data.Eq (Eq (..))
import "base" Data.Foldable (Foldable (..), and)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (..), void)
import "base" Data.Functor.Classes (Eq1 (..))
import "kan-extensions" Data.Functor.Day (Day (..))
import "base" Data.Functor.Identity (Identity (..))
import "base" Data.List (zipWith)
import "base" Data.Monoid (Monoid (..))
import "base" Data.Ord (Ord (..))
import "base" Data.Semigroup (Semigroup (..))
import "base" Numeric.Natural (Natural)
import "this" Yaya.Pattern
  ( AndMaybe (..),
    Either (..),
    Maybe (..),
    Pair (..),
    XNor (..),
    either,
    maybe,
  )
import Prelude (Integer, Integral, Num (..))

-- | Converts the free monoid (a list) into some other `Monoid`.
lowerMonoid :: Monoid m => (a -> m) -> XNor a m -> m
lowerMonoid f = \case
  Neither -> mempty
  Both a b -> f a <> b

-- | Converts the free semigroup (a non-empty list) into some other `Semigroup`.
lowerSemigroup :: Semigroup m => (a -> m) -> AndMaybe a m -> m
lowerSemigroup f = \case
  Only a -> f a
  Indeed a b -> f a <> b

-- | Converts the free monad into some other `Monad`.
lowerMonad :: Monad m => (forall x. f x -> m x) -> FreeF f a (m a) -> m a
lowerMonad f = \case
  Pure a -> pure a
  Free fm -> join (f fm)

-- | Provides equality over arbitrary pattern functors.
equal :: (Functor f, Foldable f, Eq1 f) => Day f f Bool -> Bool
equal (Day f1 f2 fn) =
  liftEq (==) (void f1) (void f2)
    && and (zipWith fn (toList f1) (toList f2))

-- TODO: Redefine this using `Natural`

-- | When folded, returns the height of the data structure.
height :: Foldable f => f Integer -> Integer
height = (+ 1) . foldr max (-1)

-- NB: It seems like this could be some more general notion of this, like
--        size :: (Foldable f, Semiring a) => f a -> a
--        size = foldr (+) one

-- | When folded, returns the number of nodes in the data structure.
--
--  __NB__: This is /not/ the same as the length when applied to a list. I.e.,
--          @`length` xs + 1 == `cata` `size` xs@, because this is counting the
--          nodes of the structure (how many `Neither`s and `Both`s), not how
--          many elements (which would be equivalent to only counting `Both`s).
size :: Foldable f => f Natural -> Natural
size = foldr (+) 1

-- | Converts a provably infinite structure into a `Yaya.Zoo.Partial` one (that
--   will never terminate).
toRight :: Identity b -> Either a b
toRight = Right . runIdentity

-- | Captures the input value if the application was undefined.
definedOrInput :: (a -> Maybe b) -> a -> Either a b
definedOrInput f a = maybe (Left a) Right $ f a

-- | Collapses a `Yaya.Zoo.Partial` structure to a value (probably requiring
--   unsafe instances).
fromEither :: Either a a -> a
fromEither = either id id

-- | Generates an infinite structure from an arbitrary seed.
never :: a -> Identity a
never = Identity

le :: Day Maybe Maybe Bool -> Bool
le = \case
  Day Nothing _ _ -> True
  Day (Just a) (Just b) f -> f a b
  Day (Just _) Nothing _ -> False

takeAnother :: Day Maybe ((,) a) b -> XNor a b
takeAnother = \case
  Day Nothing _ _ -> Neither
  Day (Just x) (h, t) f -> Both h (f x t)

takeAvailable :: Day Maybe (XNor a) b -> XNor a b
takeAvailable = \case
  Day Nothing _ _ -> Neither
  Day (Just x) t f -> fmap (f x) t

takeNext :: Day Maybe ((,) a) a -> a
takeNext = \case
  Day Nothing (h, _) _ -> h
  Day (Just x) (_, t) f -> f x t

maybeTakeNext :: Day Maybe (XNor a) (Maybe a) -> Maybe a
maybeTakeNext = \case
  Day Nothing (Both h _) _ -> Just h
  Day (Just x) (Both _ t) f -> f x t
  Day _ Neither _ -> Nothing

truncate' :: Functor f => Day Maybe f a -> FreeF f () a
truncate' = \case
  Day Nothing _ _ -> Pure ()
  Day (Just n) fa f -> Free (fmap (f n) fa)

-- | Converts a single value into a tuple with the same value on both sides.
--   > x &&& y = (x *** y) . diagonal
diagonal :: a -> Pair a a
diagonal x = x :!: x

-- * sequence generators

--
--   These functions are defined with different type parameters in order to
--   constrain the implementation, but to be used as coalgebras, all of the
--   parameters need to be specialized to the same type.

unarySequence :: (a -> b) -> a -> (a, b)
unarySequence f a = (a, f a)

binarySequence :: (a -> b -> c) -> (a, b) -> (a, (b, c))
binarySequence f (a, b) = (a, (b, f a b))

ternarySequence :: (a -> b -> c -> d) -> (a, b, c) -> (a, (b, c, d))
ternarySequence f (a, b, c) = (a, (b, c, f a b c))

lucasSequence' :: Integral i => i -> i -> (i, i) -> (i, (i, i))
lucasSequence' p q = binarySequence (\n2 n1 -> p * n1 - q * n2)
