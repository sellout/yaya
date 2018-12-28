-- | Common algebras that are useful when folding.
module Yaya.Fold.Common where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Functor
import Data.Functor.Classes
import Data.Functor.Day
import Data.Functor.Identity
import Data.Semigroup
import Numeric.Natural

import Yaya.Pattern

-- | Converts the free monoid (a list) into some other monoid.
lowerMonoid :: Monoid m => (a -> m) -> XNor a m -> m
lowerMonoid f = \case
  Neither  -> mempty
  Both a b -> mappend (f a) b

-- | Converts the free semigroup (a non-empty list) into some other semigroup.
lowerSemigroup :: Semigroup m => (a -> m) -> AndMaybe a m -> m
lowerSemigroup f = \case
  Only a     -> f a
  Indeed a b -> f a <> b

lowerMonad :: Monad m => (forall a. f a -> m a) -> FreeF f a (m a) -> m a
lowerMonad f = \case
  Pure a  -> pure a
  Free fm -> join (f fm)

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
-- | When folded, returns the number ef nodes in the data structure.
size :: Foldable f => f Natural -> Natural
size = foldr (+) 1

toRight :: Identity b -> Either a b
toRight = Right . runIdentity

-- | Returns the last 'Just' result.
while :: (a -> Maybe a) -> a -> Either a a
while f a = maybe (Left a) Right $ f a

fromEither :: Either a a -> a
fromEither = \case
  Left a  -> a
  Right a -> a

never :: a -> Identity a
never = Identity

le :: Day Maybe Maybe Bool -> Bool
le = \case
  Day Nothing  _        _ -> True
  Day (Just a) (Just b) f -> f a b
  Day (Just _) Nothing  _ -> False

takeAnother :: Day Maybe ((,) a) b -> XNor a b
takeAnother = \case
  Day Nothing  _      _ -> Neither
  Day (Just x) (h, t) f -> Both h (f x t)

takeAvailable :: Day Maybe (XNor a) b -> XNor a b
takeAvailable = \case
  Day Nothing  _ _ -> Neither
  Day (Just x) t f -> fmap (f x) t

truncate' :: Functor f => Day Maybe f a -> FreeF f () a
truncate' = \case
  Day Nothing  fa _ -> Pure ()
  Day (Just n) fa f -> Free (fmap (f n) fa)

-- | Converts a single value into a tuple with the same value on both sides.
--   > x &&& y = (x *** y) . split
split :: a -> (a, a)
split x = (x, x)

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
