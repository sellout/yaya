import Control.Monad (unless)
import Data.Foldable (and)
import Data.Traversable (sequenceA)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), IO, hSetBuffering, stderr, stdout)
import qualified Test.Fold as Fold

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <-
    sequenceA
      [ Fold.tests
      ]

  unless (and results) exitFailure
