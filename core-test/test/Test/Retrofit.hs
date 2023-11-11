-- These are both needed by `extractPatternFunctor`.
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The point of this module is that it should compile _without_ importing any
--   other Yaya modules.
module Test.Retrofit where

import "base" Data.Bool (Bool)
import "base" Data.Eq (Eq)
import "base" Data.Int (Int)
import "hedgehog" Hedgehog (checkParallel, discover)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "yaya" Yaya.Retrofit (defaultRules, extractPatternFunctor)

data DExpr
  = Lit Int
  | Add DExpr DExpr
  | Mult DExpr DExpr
  deriving (Eq, Show)

extractPatternFunctor defaultRules ''DExpr

-- -- | This can be derived in this case, but we want to ensure we could define it
-- --   if necessary.
-- instance Eq DExpr where
--   (==) = recursiveEq

-- -- | This can be derived in this case, but we want to ensure we could define it
-- --   if necessary.
-- instance Show DExpr where
--   showsPrec = recursiveShowsPrec

tests :: IO Bool
tests = checkParallel $$(discover)
