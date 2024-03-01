{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native
  ( module Yaya.Fold.Native.Internal,
    Fix (..),
    distCofreeT,
  )
where

import "base" Control.Category (Category (..))
import "base" Data.Bifunctor (Bifunctor (..))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (..))
import "base" Data.Functor.Classes (Eq1, Show1)
import "base" Data.List.NonEmpty
import "base" Numeric.Natural
import "base" Text.Show (Show (showsPrec))
import "comonad" Control.Comonad (Comonad (..))
import "comonad" Control.Comonad.Trans.Env (EnvT (..), runEnvT)
import "free" Control.Comonad.Cofree (Cofree (..), unwrap)
import "free" Control.Monad.Trans.Free (Free, FreeF (..), free)
import "strict" Data.Strict.Classes (Strict (..))
import "this" Yaya.Fold
  ( Corecursive (..),
    DistributiveLaw,
    Projectable (..),
    Recursive (..),
    Steppable (..),
    recursiveEq,
    recursiveShowsPrec,
  )
import "this" Yaya.Fold.Common (diagonal)
import "this" Yaya.Fold.Native.Internal (Cofix (unCofix))
import "this" Yaya.Pattern (AndMaybe (..), Maybe, XNor (..), uncurry)

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   strict/recursive.
newtype Fix f = Fix {unFix :: f (Fix f)}

instance Projectable (->) (Fix f) f where
  project = unFix

instance Steppable (->) (Fix f) f where
  embed = Fix

instance (Functor f) => Recursive (->) (Fix f) f where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance (Functor f, Foldable f, Eq1 f) => Eq (Fix f) where
  (==) = recursiveEq

instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

instance Recursive (->) Natural Maybe where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance Corecursive (->) [a] (XNor a) where
  ana ψ =
    ( \case
        Neither -> []
        Both h t -> h : ana ψ t
    )
      . ψ

instance Corecursive (->) (NonEmpty a) (AndMaybe a) where
  ana ψ =
    ( \case
        Only h -> h :| []
        Indeed h t -> h :| toList (ana ψ t)
    )
      . ψ

instance (Functor f) => Corecursive (->) (Free f a) (FreeF f a) where
  ana ψ =
    free
      . ( \case
            Pure a -> Pure a
            Free fb -> Free . fmap (ana ψ) $ fb
        )
      . ψ

instance (Functor f) => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . toStrict . runEnvT . ψ

distCofreeT ::
  (Functor f, Functor h) =>
  DistributiveLaw (->) f h ->
  DistributiveLaw (->) f (Cofree h)
distCofreeT k =
  ana $ uncurry EnvT . bimap (fmap extract) (k . fmap unwrap) . diagonal
