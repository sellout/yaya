{-# LANGUAGE TemplateHaskell #-}

-- | Common pattern functors (and instances for them).
module Yaya.Pattern where

import Data.Bifunctor (Bifunctor (..))
import Text.Show.Deriving (deriveShow1, deriveShow2)

-- | A version of `(,)` that is strict in its second parameter.
data Pair a b = Pair {car :: ~a, cdr :: b}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveShow1 ''Pair
deriveShow2 ''Pair

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)

-- | A strict version of `Maybe`
data Optional a = None | Some a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveShow1 ''Optional

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = Neither | Both ~a b
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveShow1 ''XNor
deriveShow2 ''XNor

instance Bifunctor XNor where
  bimap f g = \case
    Neither -> Neither
    Both a b -> Both (f a) (g b)

-- | Isomorphic to `(a, Maybe b)`, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only a | Indeed ~a b
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveShow1 ''AndMaybe
deriveShow2 ''AndMaybe

instance Bifunctor AndMaybe where
  bimap f g = \case
    Only a -> Only (f a)
    Indeed a b -> Indeed (f a) (g b)
