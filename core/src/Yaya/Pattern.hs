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

import Control.Applicative (Applicative (..))
import Control.Comonad (Comonad (..))
import Control.Monad (Monad (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable)
import Data.Function (($))
import Data.Functor (Functor)
import Data.Strict.Either -- explicitly omitted import list
import Data.Strict.Maybe -- explicitly omitted import list
import Data.Strict.Tuple -- explicitly omitted import list
import Data.Traversable (Traversable)

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = Neither | Both ~a b
  deriving (Functor, Foldable, Traversable)

instance Bifunctor XNor where
  bimap f g = \case
    Neither -> Neither
    Both a b -> Both (f a) (g b)

-- | Isomorphic to `(a, Maybe b)`, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only a | Indeed ~a b
  deriving (Functor, Foldable, Traversable)

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
