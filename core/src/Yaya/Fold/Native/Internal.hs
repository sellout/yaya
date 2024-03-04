{-# LANGUAGE Safe #-}
-- NB: We disable @StrictData@ here in order for `Cofix` to be lazy. I don’t
--     think there is any way to explicitly add @~@ patterns that has the
--     correct semantics.
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module only exists to restrict the scope of @NoStrictData@. Everything
--    is re-exported via "Yaya.Fold".
module Yaya.Fold.Native.Internal
  ( Cofix (Cofix, unCofix),
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Functor (Functor (fmap))
import "this" Yaya.Fold
  ( Corecursive (ana),
    Projectable (project),
    Steppable (embed),
  )

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   lazy/corecursive.
data Cofix f = Cofix {unCofix :: f (Cofix f)}

{-# HLINT ignore Cofix "Use newtype instead of data" #-}

instance Projectable (->) (Cofix f) f where
  project = unCofix

instance Steppable (->) (Cofix f) f where
  embed = Cofix

instance (Functor f) => Corecursive (->) (Cofix f) f where
  ana φ = embed . fmap (ana φ) . φ
