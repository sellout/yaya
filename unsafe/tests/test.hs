{-# LANGUAGE Unsafe #-}

import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold

main :: IO ()
main = defaultMain [Fold.tests]
