import           Control.Monad
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Fold as Fold
import qualified Test.Fold.Common as Fold.Common

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [ Fold.tests
                      , Fold.Common.tests
                      ]

  unless (and results) exitFailure
