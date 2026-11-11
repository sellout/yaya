{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.2.1.2
module Yaya.Hedgehog
  ( evalNonterminating,
    nonterminatingProperty,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Function (const)
import safe "base" Data.Maybe (maybe)
import "base" GHC.IO (evaluate)
import safe "base" GHC.Stack (HasCallStack)
import safe "base" System.Timeout (timeout)
import safe "base" Text.Show (Show)
import qualified "hedgehog" Hedgehog as HH

-- | Returns success if the expression doesn’t terminate, failure otherwise.
--   Termination is just checked with a 1 second timeout, so this isn’t
--   foolproof.
--
-- @since 0.2.1
evalNonterminating ::
  (HasCallStack, MonadIO m, HH.MonadTest m, Show a) => a -> m ()
evalNonterminating =
  maybe HH.success (const HH.failure <=< HH.annotateShow)
    <=< HH.evalIO . timeout 1_000_000 . evaluate
{-# INLINEABLE evalNonterminating #-}

-- | Returns success if the expression doesn’t terminate, failure otherwise.
--   The value passed here should termina
--
-- @since 0.2.1
nonterminatingProperty :: (HasCallStack, Show a) => a -> HH.Property
nonterminatingProperty = HH.withTests 1 . HH.property . evalNonterminating
