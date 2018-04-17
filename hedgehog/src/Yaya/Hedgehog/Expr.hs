{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog.Expr where

import           Data.Eq.Deriving
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Text.Show.Deriving

data Expr a
  = Lit Int
  | Add a a
  | Mult a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Expr
deriveShow1 ''Expr

genExpr :: Gen a -> Gen (Expr a)
genExpr a = Gen.frequency [ (3, Lit <$> Gen.int (Range.linear (-1000) 1000))
                          , (1, Add <$> a <*> a)
                          , (1, Mult <$> a <*> a)
                          ]
