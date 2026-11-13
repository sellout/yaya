{-# LANGUAGE Safe #-}
-- __NB__: We disable @StrictData@ here in order for `Cofix` to be lazy. I don’t
--         think there is any way to explicitly add @~@ patterns that has the
--         correct semantics.
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   This module only exists to restrict the scope of @NoStrictData@. Everything
--    is re-exported via "Yaya.Fold".
--
-- @since 0.5.0
module Yaya.Fold.Native.Internal
  ( Cofix,
  )
where

import "base" Control.Category ((.))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Classes (Read1)
import "base" Data.Kind (Type)
import "base" Text.Read (Read, readListPrec, readListPrecDefault, readPrec)
import "this" Yaya.Fold
  ( Corecursive,
    Projectable,
    Steppable,
    ana,
    embed,
    project,
    steppableReadPrec,
  )

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   lazy/corecursive.
--
-- @since 0.5.0
data Cofix (f :: Type -> Type)
  = -- | Wraps a single layer of the pattern functor.
    --
    -- @since 0.5.0
    Cofix (f (Cofix f))

type role Cofix nominal

{-# HLINT ignore Cofix "Use newtype instead of data" #-}

-- | @since 0.5.0
instance Projectable (->) (Cofix f) f where
  project (Cofix fCofix) = fCofix

-- | @since 0.5.0
instance Steppable (->) (Cofix f) f where
  embed = Cofix

-- | @since 0.5.0
instance (Functor f) => Corecursive (->) (Cofix f) f where
  ana φ = embed . fmap (ana φ) . φ
  {-# INLINEABLE ana #-}

-- | @since 0.6.1
instance (Read1 f) => Read (Cofix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault
