{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- __NB__: base-4.17 moves `IsList` to its own module, which avoids the unsafety
--         of importing "GHC.Exts". With prior versions of base, we at least
--         mark the module @Trustworthy@.
#if MIN_VERSION_base(4, 17, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- | This module /only/ contains instances, because they are viral – i.e., they
--   affect any module that imports them anywhere in their import graph. So they
--   must be quarantined.
--
--   In fact, this module should /never/ be imported. It exists for a couple
--   reasons:
--
-- - to illustrate /how/ these instances are unsafe and
-- - if you need one of these instances to satisfy something that would
--   otherwise involve a lot of code duplication, you can copy that individual
--   instance from this module, with a comment about where it came from, and
--   include it in as isolated a place as possible.
module Yaya.Native.Unsafe.Fold.Instances
  (
  )
where

import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Functor (Functor)
import safe "base" Data.Functor.Classes (Eq1, Ord1, Show1)
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.Monoid (Monoid, mconcat, mempty)
import safe "base" Data.Ord (Ord, compare)
import safe "base" Data.Semigroup (Semigroup, sconcat, stimes, (<>))
import safe "base" Text.Show (Show, showsPrec)
import safe "yaya" Yaya.Applied
  ( append,
    coappend,
    defaultMconcat,
    defaultSconcat,
  )
import safe "yaya" Yaya.Fold
  ( Corecursive,
    Mu,
    Nu,
    Recursive,
    ana,
    cata,
    embed,
    recursiveCompare,
    recursiveEq,
    recursiveShowsPrec,
  )
import safe "yaya" Yaya.Pattern (AndMaybe, XNor (Neither))
import safe qualified "yaya-unsafe" Yaya.Unsafe.Applied as Unsafe
import safe qualified "yaya-unsafe" Yaya.Unsafe.Fold as Unsafe
import safe "this" Yaya.Native.Fold (Cofix, Fix)
import safe "this" Yaya.Native.Unsafe.Fold (defaultStimes)

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList (Item, fromList, fromListN, toList))
#else
import "base" GHC.Exts (IsList (Item, fromList, fromListN, toList))
#endif

instance (Functor f) => Corecursive (->) (Fix f) f where
  ana = Unsafe.unsafeAna

instance (Functor f) => Recursive (->) (Cofix f) f where
  cata = Unsafe.unsafeCata

instance (Functor f, Foldable f, Eq1 f) => Eq (Cofix f) where
  (==) = recursiveEq
  {-# INLINEABLE (==) #-}

-- | @since 0.4.1.0
instance (Functor f, Foldable f, Ord1 f) => Ord (Cofix f) where
  compare = recursiveCompare

instance (Functor f, Show1 f) => Show (Cofix f) where
  showsPrec = recursiveShowsPrec

instance Recursive (->) (NonEmpty a) (AndMaybe a) where
  cata = Unsafe.unsafeCata

instance Recursive (->) [a] (XNor a) where
  cata = Unsafe.unsafeCata

-- | `fromList` in this instance is unsafe, but `fromListN` is safe, because we
--   have a finite length to fold.
--
--   This means that most uses of @OverloadedLists@ should be fine, but not the
--   range (`..`) syntax.
instance IsList (Fix (XNor a)) where
  type Item (Fix (XNor a)) = a
  fromList = Unsafe.unsafeFromList
  fromListN = fromListN
  toList = toList

instance Semigroup (Cofix (XNor a)) where
  (<>) = coappend
  sconcat = defaultSconcat
  stimes = defaultStimes

instance Monoid (Cofix (XNor a)) where
  mempty = embed Neither
  mconcat = defaultMconcat

-- FIXME: The implementation of `sconcat` for `Fix (XNor a)` causes compilation
--        to hang for some reason.

-- instance Semigroup (Fix (XNor a)) where
--   (<>) = append
--   sconcat = defaultSconcat
--   stimes = defaultStimes

-- instance Monoid (Fix (XNor a)) where
--   mempty = embed Neither
--   mconcat = defaultMconcat

instance Semigroup (Mu (XNor a)) where
  (<>) = append
  sconcat = defaultSconcat
  stimes = defaultStimes

instance Monoid (Mu (XNor a)) where
  mempty = embed Neither
  mconcat = defaultMconcat

instance Semigroup (Nu (XNor a)) where
  (<>) = coappend
  sconcat = defaultSconcat
  stimes = defaultStimes

instance Monoid (Nu (XNor a)) where
  mempty = embed Neither
  mconcat = defaultMconcat
