{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- | The point of this module is that it should compile _without_ importing any
--   other Yaya modules.
module Test.Native.Retrofit (tests) where

import safe "base" Data.Bool (Bool)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Int (Int)
import safe "base" Data.Ord (Ord, compare)
import safe "base" System.IO (IO)
import safe "base" Text.Read
  ( Read,
    readListPrec,
    readListPrecDefault,
    readPrec,
  )
import safe "base" Text.Show (Show, showsPrec)
import safe "deriving-compat" Data.Eq.Deriving (deriveEq1)
import safe "deriving-compat" Data.Ord.Deriving (deriveOrd1)
import safe "deriving-compat" Text.Read.Deriving (deriveRead1)
import safe "deriving-compat" Text.Show.Deriving (deriveShow1)
import safe "hedgehog" Hedgehog (checkParallel, discover)
import safe "yaya-native" Yaya.Native.Retrofit
  ( defaultRules,
    extractPatternFunctor,
    recursiveCompare,
    recursiveEq,
    recursiveShowsPrec,
    steppableReadPrec,
  )

data DExpr
  = Lit Int
  | Add DExpr DExpr
  | Mult DExpr DExpr

extractPatternFunctor defaultRules ''DExpr

deriving stock instance (Eq a) => Eq (DExprF a)

deriving stock instance (Ord a) => Ord (DExprF a)

deriving stock instance (Read a) => Read (DExprF a)

deriving stock instance (Show a) => Show (DExprF a)

deriveEq1 ''DExprF
deriveOrd1 ''DExprF
deriveRead1 ''DExprF
deriveShow1 ''DExprF

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Eq DExpr where
  (==) = recursiveEq

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Ord DExpr where
  compare = recursiveCompare

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Read DExpr where
  readPrec = steppableReadPrec
  readListPrec = readListPrecDefault

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Show DExpr where
  showsPrec = recursiveShowsPrec

tests :: IO Bool
tests = checkParallel $$discover
