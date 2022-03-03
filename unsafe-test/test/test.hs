import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified Test.Fold as Fold

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <-
    sequence
      [ Fold.tests
      ]

  unless (and results) exitFailure
