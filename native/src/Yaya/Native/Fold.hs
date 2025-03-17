{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Native.Fold
  ( module Yaya.Native.Fold.Internal,
    Fix,
    distCofreeT,
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Bifunctor (Bifunctor (bimap))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.Ord (Ord (compare))
import "base" Data.Word (Word, Word16, Word32, Word64, Word8)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read (readListPrec, readPrec), readListPrecDefault)
import "base" Text.Show (Show (showsPrec))
import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Env (EnvT (EnvT))
import "free" Control.Comonad.Cofree (Cofree, unwrap)
import "free" Control.Monad.Trans.Free (Free, FreeF)
import "yaya" Yaya.Fold
  ( Corecursive,
    DistributiveLaw,
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
import "yaya" Yaya.Fold.Common (diagonal)
import "yaya" Yaya.Pattern (AndMaybe, Maybe, XNor, uncurry)
import qualified "yaya-unsafe" Yaya.Unsafe.Fold as Unsafe
import "this" Yaya.Native.Fold.Internal (Cofix)

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   strict/recursive.
newtype Fix f = Fix (f (Fix f))

instance Projectable (->) (Fix f) f where
  project (Fix fFf) = fFf

instance Steppable (->) (Fix f) f where
  embed = Fix

instance (Functor f) => Recursive (->) (Fix f) f where
  cata = Unsafe.unsafeCata

instance (Functor f, Foldable f, Eq1 f) => Eq (Fix f) where
  (==) = recursiveEq

-- | @since 0.6.1.0
instance (Functor f, Foldable f, Ord1 f) => Ord (Fix f) where
  compare = recursiveCompare

-- | @since 0.6.1.0
instance (Read1 f) => Read (Fix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

instance Recursive (->) Natural Maybe where
  cata = Unsafe.unsafeCata

instance Recursive (->) Word Maybe where
  cata = Unsafe.unsafeCata

instance Recursive (->) Word16 Maybe where
  cata = Unsafe.unsafeCata

instance Recursive (->) Word32 Maybe where
  cata = Unsafe.unsafeCata

instance Recursive (->) Word64 Maybe where
  cata = Unsafe.unsafeCata

instance Recursive (->) Word8 Maybe where
  cata = Unsafe.unsafeCata

instance Corecursive (->) [a] (XNor a) where
  ana = Unsafe.unsafeAna

instance Corecursive (->) (NonEmpty a) (AndMaybe a) where
  ana = Unsafe.unsafeAna

instance (Functor f) => Corecursive (->) (Free f a) (FreeF f a) where
  ana = Unsafe.unsafeAna

instance (Functor f) => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana = Unsafe.unsafeAna

distCofreeT ::
  (Functor f, Functor h) =>
  DistributiveLaw (->) f h ->
  DistributiveLaw (->) f (Cofree h)
distCofreeT k =
  ana $ uncurry EnvT . bimap (fmap extract) (k . fmap unwrap) . diagonal
