{-# LANGUAGE Unsafe #-}

module Main (main) where

import safe "base" Data.Function (($))
import safe "base" Data.Semigroup (Semigroup ((<>)))
import safe "base" System.IO (IO)
import "doctest" Test.DocTest (doctest)
import "this" Build_doctests (flags, module_sources, pkgs)

main :: IO ()
main = doctest $ flags <> pkgs <> module_sources
