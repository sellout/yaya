{-# LANGUAGE NumericUnderscores #-}

module Yaya.Hedgehog where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import GHC.IO (evaluate)
import GHC.Stack (HasCallStack)
import Hedgehog
import System.Timeout (timeout)

-- | Returns success if the expression doesn’t terminate, failure otherwise.
--   Termination is just checked with a 1 second timeout, so this isn’t
--   foolproof.
evalNonterminating ::
  (HasCallStack, MonadIO m, MonadTest m, Show a) => a -> m ()
evalNonterminating =
  maybe success (const failure <=< annotateShow)
    <=< evalIO . timeout 1_000_000 . evaluate

-- | Returns success if the expression doesn’t terminate, failure otherwise.
--   The value passed here should termina
nonterminatingProperty :: (HasCallStack, Show a) => a -> Property
nonterminatingProperty = withTests 1 . property . evalNonterminating
