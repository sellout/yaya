-- __NB__: `custom-setup` doesn’t have any way to specify extensions, so any we
--         want need to be specified here.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import safe "base" System.IO (IO)
import "cabal-doctest" Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctests"
