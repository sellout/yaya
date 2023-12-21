import "base" System.IO (IO)
import "hedgehog" Hedgehog.Main (defaultMain)
import qualified "this" Test.Fold as Fold

main :: IO ()
main = defaultMain [Fold.tests]
