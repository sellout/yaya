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

module Yaya.Unsafe.Applied () where

import "yaya" Yaya.Fold (Mu)
import "yaya" Yaya.Fold.Native (Fix)
import "yaya" Yaya.Pattern (XNor)
import "this" Yaya.Unsafe.Fold (Unsafe)

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList (Item, fromList, fromListN, toList))
#else
import "base" GHC.Exts (IsList (Item, fromList, fromListN, toList))
#endif

-- | `fromList` in this instance is unsafe, but `fromListN` is safe, because we
--   have a finite length to fold.
--
--   This means that most uses of @OverloadedLists@ should be fine, but not the
--   range (`..`) syntax.
instance IsList (Unsafe (Fix (XNor a))) where
  type Item (Unsafe (Fix (XNor a))) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList

-- | `fromList` in this instance is unsafe, but `fromListN` is safe, because we
--   have a finite length to fold.
--
--   This means that most uses of @OverloadedLists@ should be fine, but not the
--   range (`..`) syntax.
instance IsList (Unsafe (Mu (XNor a))) where
  type Item (Unsafe (Mu (XNor a))) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList
