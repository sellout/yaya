{-# LANGUAGE Unsafe #-}

module Yaya.Hedgehog where

import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Function (const)
import safe "base" Data.Maybe (maybe)
import "base" GHC.IO (evaluate)
import safe "base" GHC.Stack (HasCallStack)
import safe "base" System.Timeout (timeout)
import safe "base" Text.Show (Show)
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
