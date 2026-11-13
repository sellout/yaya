{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.3.3
module Yaya.Unsafe.Applied
  ( unsafeFromList,
  )
where

import "yaya" Yaya.Fold (Steppable, embed)
import "yaya" Yaya.Pattern (XNor)
import "this" Yaya.Unsafe.Fold (unsafeCata)

-- | An unsafe implementation of `GHC.Exts.fromList` for `Steppable`
--   fixed-points of `XNor`.
--
-- @since 0.3.3
unsafeFromList :: (Steppable (->) t (XNor a)) => [a] -> t
unsafeFromList = unsafeCata embed
