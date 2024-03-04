{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native
  ( module Yaya.Fold.Native.Internal,
    Fix (Fix, unFix),
    distCofreeT,
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Bifunctor (Bifunctor (bimap))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable (toList))
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord (compare))
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read (readListPrec, readPrec), readListPrecDefault)
import "base" Text.Show (Show (showsPrec))
import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Env (EnvT (EnvT), runEnvT)
import "free" Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import "free" Control.Monad.Trans.Free (Free, FreeF (Free, Pure), free)
import "strict" Data.Strict.Classes (Strict (toStrict))
import "this" Yaya.Fold
  ( Corecursive (ana),
    DistributiveLaw,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    recursiveCompare,
    recursiveEq,
    recursiveShowsPrec,
    steppableReadPrec,
  )
import "this" Yaya.Fold.Common (diagonal)
import "this" Yaya.Fold.Native.Internal (Cofix (unCofix))
import "this" Yaya.Pattern
  ( AndMaybe (Indeed, Only),
    Maybe,
    XNor (Both, Neither),
    uncurry,
  )

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

-- | @since 0.5.3.0
instance (Functor f, Foldable f, Ord1 f) => Ord (Fix f) where
  compare = recursiveCompare

-- | @since 0.5.3.0
instance (Read1 f) => Read (Fix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

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
        Indeed h t -> h :| toList @NonEmpty (ana ψ t)
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
