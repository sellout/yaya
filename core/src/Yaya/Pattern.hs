{-# OPTIONS_GHC -Wno-orphans #-}

-- | Common pattern functors (and instances for them).
--
--   This re-exports the functors from the strict library because it also adds
--   some orphan instances for them.
module Yaya.Pattern
  ( module Data.Strict.Either,
    module Data.Strict.Maybe,
    module Data.Strict.Tuple,
    XNor (..),
    AndMaybe (..),
  )
where

import "base" Control.Applicative (Applicative (..))
import "base" Control.Category (Category ((.)))
import "base" Control.Monad (Monad (..))
import "base" Data.Bifunctor (Bifunctor (..))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor)
import "base" Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2 (liftEq2),
    Ord1 (liftCompare),
    Ord2 (liftCompare2),
    Show1 (liftShowsPrec),
    Show2 (liftShowsPrec2),
  )
import "base" Data.Ord (Ord (compare, (<=)), Ordering (EQ, GT, LT))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" Text.Show (Show (showList, showsPrec), showParen, showString)
import "comonad" Control.Comonad (Comonad (..))
-- explicitly omitted import list for @strict@ modules
import "strict" Data.Strict.Either
import "strict" Data.Strict.Maybe
import "strict" Data.Strict.Tuple
import "base" Prelude (Num ((+)))

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = Neither | Both ~a b
  deriving (Eq, Generic, Ord, Show, Foldable, Functor, Generic1, Traversable)

instance (Eq a) => Eq1 (XNor a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftEq = liftEq2 (==)

instance Eq2 XNor where
  liftEq2 f g = Tuple.curry $ \case
    (Neither, Neither) -> True
    (Both x y, Both x' y') -> f x x' && g y y'
    (_, _) -> False

instance (Ord a) => Ord1 (XNor a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftCompare = liftCompare2 compare

instance Ord2 XNor where
  liftCompare2 f g = Tuple.curry $ \case
    (Neither, Neither) -> EQ
    (Neither, Both _ _) -> LT
    (Both _ _, Neither) -> GT
    (Both x y, Both x' y') -> f x x' <> g y y'

instance (Show a) => Show1 (XNor a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 XNor where
  liftShowsPrec2 showsPrecX _showListX showsPrecY _showListY prec =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          Neither -> showString "Neither"
          Both x y ->
            showParen (nextPrec <= prec) $
              showString "Both "
                . showsPrecX nextPrec x
                . showString " "
                . showsPrecY nextPrec y

instance Bifunctor XNor where
  bimap f g = \case
    Neither -> Neither
    Both a b -> Both (f a) (g b)

-- | Isomorphic to `(a, Maybe b)`, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only ~a | Indeed ~a b
  deriving (Eq, Generic, Show, Foldable, Functor, Generic1, Traversable)

instance (Eq a) => Eq1 (AndMaybe a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftEq = liftEq2 (==)

instance Eq2 AndMaybe where
  liftEq2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Indeed x y, Indeed x' y') -> f x x' && g y y'
    (_, _) -> False

-- | This definition is different from the one that is derivable. For example,
--   the derived instance would always have
--   @`compare` (`Only` x) (`Indeed` x' y) `==` `LT`@, but this instance will
--   return `GT` if @`compare` x x' `==` `GT`@.
instance (Ord a, Ord b) => Ord (AndMaybe a b) where
  compare = liftCompare compare

instance (Ord a) => Ord1 (AndMaybe a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftCompare = liftCompare2 compare

instance Ord2 AndMaybe where
  liftCompare2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Only x, Indeed x' _) -> f x x' <> LT
    (Indeed x _, Only x') -> f x x' <> GT
    (Indeed x y, Indeed x' y') -> f x x' <> g y y'

instance (Show a) => Show1 (AndMaybe a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 AndMaybe where
  liftShowsPrec2 showsPrecX _showListX showsPrecY _showListY prec =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          Only x ->
            showParen (nextPrec <= prec) $
              showString "Only " . showsPrecX nextPrec x
          Indeed x y ->
            showParen (nextPrec <= prec) $
              showString "Indeed "
                . showsPrecX nextPrec x
                . showString " "
                . showsPrecY nextPrec y

instance Bifunctor AndMaybe where
  bimap f g = \case
    Only a -> Only (f a)
    Indeed a b -> Indeed (f a) (g b)

-- * orphan instances for types from the strict library

-- TODO: Explain why these instances are actually legit (fast & loose).

instance Applicative (Either a) where
  pure = Right
  liftA2 f = curry $ \case
    Right x :!: Right y -> Right $ f x y
    Right _ :!: Left a -> Left a
    Left a :!: _ -> Left a

instance Monad (Either a) where
  Left a >>= _ = Left a
  Right b >>= f = f b

instance Applicative Maybe where
  pure = Just
  liftA2 f = curry $ \case
    Just x :!: Just y -> Just $ f x y
    _ :!: _ -> Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a >>= f = f a

instance Comonad (Pair a) where
  extract = snd
  duplicate x@(a :!: _) = a :!: x
