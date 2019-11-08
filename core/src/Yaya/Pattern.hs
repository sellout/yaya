{-# LANGUAGE StrictData #-}

-- | Common pattern functors (and instances for them).
module Yaya.Pattern where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Control.Monad.Trans.Free
import Data.Bifunctor
import Data.Bitraversable
import Data.Distributive
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Day
import Data.Functor.Identity
import Data.Void
import Numeric.Natural

-- | Isomorphic to 'Maybe (a, b)', it’s also the pattern functor for lists.
data XNor a b = Neither | Both ~a b deriving (Functor, Foldable, Traversable)

instance Bifunctor XNor where
  bimap f g = \case
    Neither  -> Neither
    Both a b -> Both (f a) (g b)

-- | Isomorphic to `(a, Maybe b)`, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only a | Indeed ~a b
  deriving (Functor, Foldable, Traversable)

instance Bifunctor AndMaybe where
  bimap f g = \case
    Only a -> Only (f a)
    Indeed a b -> Indeed (f a) (g b)

