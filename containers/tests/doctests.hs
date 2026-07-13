{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2017-2020 Oleg Grenrus, 2020- Max Ulidtko
-- License: BSD-3-Clause
--
-- Test-suite driver, adapted from
-- [cabal-doctest](https://hackage.haskell.org/package/cabal-doctest).
module Main (main) where

import safe "base" Control.Category ((.))
import safe "base" Data.Foldable (fold)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.Traversable (for)
import safe "base" System.IO (IO, print, putStrLn)
import "doctest" Test.DocTest (doctest)
import "this" Build_doctests (Component (Component), components)

-- | The doctest entry point.
--
-- @since 0.0.1
main :: IO ()
main = fmap fold . for components $ \(Component name flags pkgs sources) -> do
  print name
  putStrLn "----------------------------------------"
  doctest $ flags <> pkgs <> sources
