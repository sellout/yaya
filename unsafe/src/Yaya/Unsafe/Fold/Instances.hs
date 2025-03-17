{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- __NB__: base-4.17 moves `IsList` to its own module, which avoids the unsafety
--         of importing "GHC.Exts". With prior versions of base, we at least
--         mark the module @Trustworthy@.
#if MIN_VERSION_base(4, 17, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-- | Type class instances that use direct recursion in a potentially partial
--   way. This is separated from the rest of `Yaya.Unsafe.Fold` because you can
--   neither control nor qualify the import of instances. Therefore this module
--   is /extra/ dangerous, as having these instances available applies to the
--   entire module they’re imported into.
--
--   This contains instances that you might /expect/ to see, but which aren’t
--   actually total. For example, folding a lazy list @[a]@ is /not/ guaranteed
--   to terminate.
--
--   This module /only/ contains instances, because they are viral – i.e., they
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
module Yaya.Unsafe.Fold.Instances
  (
  )
where

import safe "base" Data.Eq (Eq ((==)))
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Functor (Functor)
import safe "base" Data.Functor.Classes (Eq1, Ord1, Show1)
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.Ord (Ord (compare))
import safe "base" Text.Show (Show (showsPrec))
import safe "comonad" Control.Comonad.Env (EnvT)
import safe "free" Control.Comonad.Cofree (Cofree)
import safe "free" Control.Monad.Trans.Free (Free, FreeF)
import safe "yaya" Yaya.Fold
  ( Corecursive,
    Mu,
    Nu,
    Recursive,
    ana,
    cata,
    recursiveCompare,
    recursiveEq,
    recursiveShowsPrec,
  )
import safe "yaya" Yaya.Pattern (AndMaybe, XNor)
import safe "this" Yaya.Unsafe.Applied (unsafeFromList)
import safe qualified "this" Yaya.Unsafe.Fold as Unsafe

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList (Item, fromList, fromListN, toList))
#else
import "base" GHC.Exts (IsList (Item, fromList, fromListN, toList))
#endif

instance (Functor f) => Corecursive (->) (Mu f) f where
  ana = Unsafe.unsafeAna

instance (Functor f) => Recursive (->) (Nu f) f where
  cata = Unsafe.unsafeCata

instance (Functor f, Foldable f, Eq1 f) => Eq (Nu f) where
  (==) = recursiveEq

-- | @since 0.4.1.0
instance (Functor f, Foldable f, Ord1 f) => Ord (Nu f) where
  compare = recursiveCompare

instance (Functor f, Show1 f) => Show (Nu f) where
  showsPrec = recursiveShowsPrec

instance Recursive (->) [a] (XNor a) where
  cata = Unsafe.unsafeCata

instance Recursive (->) (NonEmpty a) (AndMaybe a) where
  cata = Unsafe.unsafeCata

instance (Functor f) => Recursive (->) (Cofree f a) (EnvT a f) where
  cata = Unsafe.unsafeCata

instance (Functor f) => Recursive (->) (Free f a) (FreeF f a) where
  cata = Unsafe.unsafeCata

-- | `fromList` in this instance is unsafe, but `fromListN` is safe, because we
--   have a finite length to fold.
--
--   This means that most uses of @OverloadedLists@ should be fine, but not the
--   range (`..`) syntax.
instance IsList (Mu (XNor a)) where
  type Item (Mu (XNor a)) = a
  fromList = unsafeFromList
  fromListN = fromListN
  toList = toList
