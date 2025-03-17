{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Yaya.Native.Unsafe.Fold
  ( defaultStimes,
  )
where

import "base" Control.Category ((.))
import "base" Data.Semigroup (Semigroup)
import "base" Numeric.Natural (Natural)
import qualified "yaya" Yaya.Applied as Yaya
import "yaya" Yaya.Fold (Mu)
import "yaya" Yaya.Pattern (Log2, maybe)
import "this" Yaya.Native.Fold ()
import "base" Prelude (Integral, errorWithoutStackTrace, fromIntegral)

-- | This requires something that implements `Num`, which currently means
--  `Natural`. And it’s partial, because it accepts any `Integral`, but only
--   works for positive numbers. See `Yaya.defaultStimes` for a version without
--   these issues.
defaultStimes :: (Semigroup a, Integral b) => b -> a -> a
defaultStimes =
  maybe (errorWithoutStackTrace "Should have been positive!") Yaya.defaultStimes
    . Yaya.toPositive @(Mu Log2) @Natural
    . fromIntegral
