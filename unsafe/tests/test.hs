{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.3.3.1
module Main (main) where

import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold

-- | Runs the property tests.
--
-- @since 0.3.3.1
main :: IO ()
main = defaultMain [Fold.tests]
