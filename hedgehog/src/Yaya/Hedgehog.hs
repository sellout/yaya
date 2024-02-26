{-# LANGUAGE NumericUnderscores #-}

module Yaya.Hedgehog where

import "base" Control.Category (Category ((.)))
import "base" Control.Monad ((<=<))
import "base" Control.Monad.IO.Class (MonadIO)
import "base" Data.Function (const)
import "base" Data.Maybe (maybe)
import "base" GHC.IO (evaluate)
import "base" GHC.Stack (HasCallStack)
import "base" System.Timeout (timeout)
import "base" Text.Show (Show)
import "hedgehog" Hedgehog

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
