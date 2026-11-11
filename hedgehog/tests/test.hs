{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.2.1.3
module Main (main) where

import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold
import qualified "this" Test.Fold.Common as Fold.Common
import qualified "this" Test.Fold.Native as Fold.Native
import qualified "this" Test.Retrofit as Retrofit

-- | Runs the property tests.
--
-- @since 0.2.1.3
main :: IO ()
main =
  defaultMain
    [Fold.tests, Fold.Common.tests, Fold.Native.tests, Retrofit.tests]
