import "base" System.IO (IO)
import "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold
import qualified "this" Test.Fold.Common as Fold.Common
import qualified "this" Test.Retrofit as Retrofit

main :: IO ()
main = defaultMain [Fold.tests, Fold.Common.tests, Retrofit.tests]
