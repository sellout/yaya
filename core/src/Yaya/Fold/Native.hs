{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
--
-- @since 0.1.0
module Yaya.Fold.Native
  ( module Yaya.Fold.Native.Internal,
    Fix,
  )
where

import "base" Control.Category ((.))
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable, toList)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord, compare)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read, readListPrec, readListPrecDefault, readPrec)
import "base" Text.Show (Show, showsPrec)
import "comonad" Control.Comonad.Trans.Env (EnvT, runEnvT)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "free" Control.Monad.Trans.Free (Free, FreeF (Free, Pure), free)
import "strict" Data.Strict.Classes (toStrict)
import "this" Yaya.Fold
  ( Corecursive,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    embed,
    project,
    recursiveCompare,
    recursiveEq,
    recursiveShowsPrec,
    steppableReadPrec,
  )
import "this" Yaya.Fold.Native.Internal (Cofix)
import "this" Yaya.Pattern
  ( AndMaybe (Indeed, Only),
    Maybe,
    XNor (Both, Neither),
    uncurry,
  )

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   strict/recursive.
--
-- @since 0.1.0
newtype Fix f
  = -- | Wraps a single layer of the pattern functor.
    --
    -- @since 0.1.0
    Fix (f (Fix f))

type role Fix nominal

-- | @since 0.1.0
instance Projectable (->) (Fix f) f where
  project (Fix fFix) = fFix

-- | @since 0.1.0
instance Steppable (->) (Fix f) f where
  embed = Fix

-- | @since 0.5.0
instance (Functor f) => Recursive (->) (Fix f) f where
  cata ɸ = ɸ . fmap (cata ɸ) . project
  {-# INLINEABLE cata #-}

-- | @since 0.5.0
instance (Functor f, Foldable f, Eq1 f) => Eq (Fix f) where
  (==) = recursiveEq

-- | @since 0.6.1
instance (Functor f, Foldable f, Ord1 f) => Ord (Fix f) where
  compare = recursiveCompare

-- | @since 0.6.1
instance (Read1 f) => Read (Fix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

-- | @since 0.5.0
instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

-- | @since 0.1.0
instance Recursive (->) Natural Maybe where
  cata ɸ = ɸ . fmap (cata ɸ) . project

-- | @since 0.1.0
instance Corecursive (->) [a] (XNor a) where
  ana ψ =
    ( \case
        Neither -> []
        Both h t -> h : ana ψ t
    )
      . ψ

-- | @since 0.1.0
instance Corecursive (->) (NonEmpty a) (AndMaybe a) where
  ana ψ =
    ( \case
        Only h -> h :| []
        Indeed h t -> h :| toList @NonEmpty (ana ψ t)
    )
      . ψ

-- | @since 0.1.0
instance (Functor f) => Corecursive (->) (Free f a) (FreeF f a) where
  ana ψ =
    free
      . ( \case
            Pure a -> Pure a
            Free fb -> Free . fmap (ana ψ) $ fb
        )
      . ψ

-- | @since 0.1.0
instance (Functor f) => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . toStrict . runEnvT . ψ
