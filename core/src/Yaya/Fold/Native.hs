{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native
  ( module Yaya.Fold.Native.Internal,
    Fix,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool (True))
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable, toList)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, (<$>))
import "base" Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Ord (Ord, compare)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read, readListPrec, readListPrecDefault, readPrec)
import "base" Text.Show (Show, showsPrec)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "free" Control.Monad.Trans.Free (Free, FreeF (Free, Pure), free)
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
    EnvT,
    Maybe,
    XNor (Both, Neither),
    runEnvT,
    uncurry,
  )
import "this" Yaya.Strict (IsNonStrict, IsStrict, Strict)

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   strict/recursive.
newtype Fix f = Fix (f (Fix f))

type instance Strict Fix = 'True

type instance Strict (Fix f) = Strict f

instance Projectable (->) (Fix f) f where
  project (Fix fFix) = fFix

instance Steppable (->) (Fix f) f where
  embed = Fix

-- |
--
--  __TODO__: @`IsStrict` (`Fix` f)@ should be implied by @`IsStrict` f@.
instance
  (IsStrict f, IsStrict (Fix f), Functor f) =>
  Recursive (->) (Fix f) f
  where
  cata ɸ = ɸ . fmap (cata ɸ) . project

-- | When the pattern functor is not `Strict`, `Fix` may be used corecursively.
instance (IsNonStrict (Fix f), Functor f) => Corecursive (->) (Fix f) f where
  ana ψ = embed . fmap (ana ψ) . ψ

instance
  (Recursive (->) (Fix f) f, Functor f, Foldable f, Eq1 f) =>
  Eq (Fix f)
  where
  (==) = recursiveEq

-- | @since 0.6.1.0
instance
  (Recursive (->) (Fix f) f, Functor f, Foldable f, Ord1 f) =>
  Ord (Fix f)
  where
  compare = recursiveCompare

-- | @since 0.6.1.0
instance (Read1 f) => Read (Fix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

instance (Recursive (->) (Fix f) f, Functor f, Show1 f) => Show (Fix f) where
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
            Free fb -> Free $ ana ψ <$> fb
        )
      . ψ

instance (Functor f) => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . runEnvT . ψ
