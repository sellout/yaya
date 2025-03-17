{-# LANGUAGE Unsafe #-}

import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Native.Fold as Fold
import qualified "this" Test.Native.Retrofit as Retrofit

main :: IO ()
main = defaultMain [Fold.tests, Retrofit.tests]
