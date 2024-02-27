module Main (main) where

import "base" Data.Function (($))
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" System.IO (IO)
import "doctest" Test.DocTest (doctest)
import "this" Build_doctests (flags, module_sources, pkgs)

main :: IO ()
main = doctest $ flags <> pkgs <> module_sources
