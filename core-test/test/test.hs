import Control.Monad (unless)
import Data.Foldable (and)
import Data.Traversable (sequenceA)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), IO, hSetBuffering, stderr, stdout)
import qualified Test.Fold as Fold
import qualified Test.Fold.Common as Fold.Common
import qualified Test.Retrofit as Retrofit

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <-
    sequenceA
      [ Fold.tests,
        Fold.Common.tests,
        Retrofit.tests
      ]

  unless (and results) exitFailure
