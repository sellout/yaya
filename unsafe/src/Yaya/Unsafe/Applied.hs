{-# LANGUAGE Safe #-}

module Yaya.Unsafe.Applied
  ( unsafeFromList,
  )
where

import "yaya" Yaya.Fold (Steppable, embed)
import "yaya" Yaya.Pattern (XNor)
import "this" Yaya.Unsafe.Fold (unsafeCata)

-- | An unsafe implementation of `GHC.Exts.fromList` for `Steppable`
--   fixed-points of `XNor`.
unsafeFromList :: (Steppable (->) t (XNor a)) => [a] -> t
unsafeFromList = unsafeCata embed
