{-# LANGUAGE Safe #-}
-- __NB__: We disable @StrictData@ here in order for `Cofix` to be lazy. I don’t
--         think there is any way to explicitly add @~@ patterns that has the
--         correct semantics.
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module only exists to restrict the scope of @NoStrictData@. Everything
--    is re-exported via "Yaya.Fold".
module Yaya.Fold.Native.Internal
  ( Cofix,
  )
where

import "base" Control.Category ((.))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Classes (Read1)
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
data Cofix f = Cofix (f (Cofix f))

{-# HLINT ignore Cofix "Use newtype instead of data" #-}

instance Projectable (->) (Cofix f) f where
  project (Cofix fCofix) = fCofix

instance Steppable (->) (Cofix f) f where
  embed = Cofix

instance (Functor f) => Corecursive (->) (Cofix f) f where
  ana φ = embed . fmap (ana φ) . φ

-- | @since 0.6.1.0
instance (Read1 f) => Read (Cofix f) where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault
