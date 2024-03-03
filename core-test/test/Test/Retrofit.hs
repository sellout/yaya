{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- | The point of this module is that it should compile _without_ importing any
--   other Yaya modules.
module Test.Retrofit (tests) where

import safe "base" Data.Bool (Bool)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Int (Int)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import safe "hedgehog" Hedgehog (checkParallel, discover)
import safe "yaya" Yaya.Retrofit (defaultRules, extractPatternFunctor)

data DExpr
  = Lit Int
  | Add DExpr DExpr
  | Mult DExpr DExpr
  deriving stock (Eq, Show)

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
tests = checkParallel $$discover
