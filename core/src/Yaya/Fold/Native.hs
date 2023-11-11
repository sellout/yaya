{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native where

import "base" Control.Applicative (Applicative)
import "base" Control.Category (Category (..))
import "comonad" Control.Comonad (Comonad (..))
import "free" Control.Comonad.Cofree (Cofree (..), unwrap)
import "comonad" Control.Comonad.Trans.Env (EnvT (..), runEnvT)
import "free" Control.Monad.Trans.Free (Free, FreeF (..), free)
import "base" Data.Bifunctor (Bifunctor (..))
import "base" Data.Bool (Bool)
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (..))
import "base" Data.List.NonEmpty
import "base" Data.Monoid (Monoid)
import "base" Data.Ord (Ord)
import "strict" Data.Strict.Classes (Strict (..))
import "base" Numeric.Natural
import "base" Text.Show (Show)
import "this" Yaya.Fold
  ( Corecursive (..),
    DistributiveLaw,
    Projectable (..),
    Recursive (..),
    Steppable (..),
  )
import "this" Yaya.Fold.Common (diagonal)
import "this" Yaya.Pattern (AndMaybe (..), Maybe, XNor (..), uncurry)
import "base" Prelude (Integral)

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   lazy/corecursive.
newtype Fix f = Fix {unFix :: f (Fix f)}

instance Projectable (->) (Fix f) f where
  project = unFix

instance Steppable (->) (Fix f) f where
  embed = Fix

instance Functor f => Corecursive (->) (Fix f) f where
  ana φ = embed . fmap (ana φ) . φ

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

instance Functor f => Corecursive (->) (Free f a) (FreeF f a) where
  ana ψ =
    free
      . ( \case
            Pure a -> Pure a
            Free fb -> Free . fmap (ana ψ) $ fb
        )
      . ψ

instance Functor f => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . toStrict . runEnvT . ψ

distCofreeT ::
  (Functor f, Functor h) =>
  DistributiveLaw (->) f h ->
  DistributiveLaw (->) f (Cofree h)
distCofreeT k =
  ana $ uncurry EnvT . bimap (fmap extract) (k . fmap unwrap) . diagonal
