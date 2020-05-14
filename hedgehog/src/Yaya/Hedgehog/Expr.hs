{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog.Expr where

import           Data.Eq.Deriving
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Text.Show.Deriving

import Yaya.Fold
import Yaya.Fold.Native
import Yaya.Hedgehog.Fold

data Expr a
  = Lit Int
  | Add a a
  | Mult a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Expr
deriveShow1 ''Expr

genExprLit :: Gen (Expr a)
genExprLit = Lit <$> Gen.int (Range.linear (-1000) 1000)

genExprOp :: Gen a -> Gen (Expr a)
genExprOp a = Gen.choice [Add <$> a <*> a, Mult <$> a <*> a]

genExpr :: Gen a -> Gen (Expr a)
genExpr a = Gen.frequency [(3, genExprLit), (2, genExprOp a)]

expression :: Steppable (->) t Expr => Size -> Gen t
expression = embeddableOfHeight genExprLit genExpr

genMuExpr :: Size -> Gen (Mu Expr)
genMuExpr = expression

genNuExpr :: Size -> Gen (Nu Expr)
genNuExpr = expression

genFixExpr :: Size -> Gen (Fix Expr)
genFixExpr = expression
