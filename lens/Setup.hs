-- __NB__: `custom-setup` doesn’t have any way to specify extensions or options,
--         so any we want need to be specified here.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Weverything #-}
-- Warns even when `Unsafe` is explicit, not inferred. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16689
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import safe "base" System.IO (IO)
import "cabal-doctest" Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctests"
