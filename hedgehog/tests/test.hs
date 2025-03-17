{-# LANGUAGE Unsafe #-}

import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold
import qualified "this" Test.Fold.Common as Fold.Common

main :: IO ()
main = defaultMain [Fold.tests, Fold.Common.tests]
