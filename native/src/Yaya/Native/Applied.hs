{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
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

module Yaya.Native.Applied
  ( fromListN,
    toList,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Int (Int)
import safe "base" Numeric.Natural (Natural)
import safe "yaya" Yaya.Applied (fromList)
import safe "yaya" Yaya.Fold
  ( Nu,
    Projectable,
    Steppable,
    ana,
    cata2,
    embed,
    project,
  )
import safe "yaya" Yaya.Fold.Common (takeAvailable)
import safe "yaya" Yaya.Pattern (XNor)
import safe "this" Yaya.Native.Fold (Cofix)
import safe "base" Prelude (fromIntegral)

-- See comment on @{-# LANGUAGE Safe #-}@ above.
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.IsList (IsList)
import qualified "base" GHC.IsList as IsList
#else
import "base" GHC.Exts (IsList)
import qualified "base" GHC.Exts as IsList
#endif

-- -- | This is a recursion-free implementation of `stimes`.
-- defaultStimes :: (Semigroup a, Integral b) => b -> a -> a
-- defaultStimes =
--   maybe
--     (errorWithoutStackTrace "stimes: positive multiplier expected")
--     (flip $ stimes' (<>))
--     . toPositive @(Mu Log2)
--     . fromIntegral @_ @(Mu Maybe)

-- | An implementation of `IsList.fromListN` for `Steppable` fixed-points of
--  `XNor`.
--
--   This should return an empty structure if the `Int` is negative.
--
--   If the target structure isn’t `Steppable` or the target structure is
--  `Corecursive` (i.e., `Yaya.Unsafe.Fold.Applied.unsafeFromList` isn’t used),
--   then the default definition for `fromListN` should suffice.
fromListN :: (Steppable (->) t (XNor a)) => Int -> [a] -> t
fromListN = cata2 (embed . takeAvailable) . fromIntegral @_ @Natural

-- | An implementation of `IsList.toList` for `Projectable` fixed-points of
--  `XNor`.
toList :: (Projectable (->) t (XNor a)) => t -> [a]
toList = ana project

-- $IsList
--
-- The `IsList` interface requires expanding to a lazy list, which requires
-- native recursion; and folding an Int, which is partial.

-- | This instance is safe, since both structures are lazy.
--
--   $IsList
instance IsList (Cofix (XNor a)) where
  type Item (Cofix (XNor a)) = a
  fromList = fromList
  toList = toList

-- | This instance is safe, since both structures are lazy.
--
--   $IsList
instance IsList (Nu (XNor a)) where
  type Item (Nu (XNor a)) = a
  fromList = fromList
  toList = toList
