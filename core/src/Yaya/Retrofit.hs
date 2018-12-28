-- | This module re-exports a subset of `Yaya.Fold`, intended for when you want
--   to define recursion scheme instances for your existing recursive types.
module Yaya.Retrofit
  ( module Yaya.Fold
  ) where

import Yaya.Fold
       ( Corecursive
       , Projectable
       , Recursive
       , recursiveEq
       , recursiveShowsPrec
       , Steppable
       )
