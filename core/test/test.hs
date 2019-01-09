import           Control.Monad
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Fold as Fold
import qualified Test.Fold.Common as Fold.Common
import qualified Test.Fold.Retrofit as Fold.Retrofit

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [ Fold.tests
                      , Fold.Common.tests
                      , Fold.Retrofit.tests
                      ]

  unless (and results) exitFailure
