-- __NB__: `custom-setup` doesn’t have any way to specify extensions or options,
--         so any we want need to be specified here.
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 0, 0)
{-# OPTIONS_GHC -Weverything #-}
-- Warns even when `Unsafe` is explicit, not inferred. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16689
{-# OPTIONS_GHC -Wno-unsafe #-}
#else
{-# OPTIONS_GHC -Wall #-}
#endif

module Main (main) where

import safe "base" System.IO (IO)
import "cabal-doctest" Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctests"
