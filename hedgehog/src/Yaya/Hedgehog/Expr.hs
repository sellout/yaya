{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

module Yaya.Hedgehog.Expr where

import safe "base" Control.Applicative (Applicative (..))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Functor (Functor, (<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.Traversable (Traversable)
import safe "base" Text.Show (Show)
import safe "deriving-compat" Data.Eq.Deriving (deriveEq1)
import safe "deriving-compat" Text.Show.Deriving (deriveShow1)
import safe "hedgehog" Hedgehog (Gen, Size)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe qualified "hedgehog" Hedgehog.Range as Range
import safe "yaya" Yaya.Fold (Mu, Nu, Steppable)
import safe "yaya" Yaya.Fold.Native (Cofix, Fix)
import safe "this" Yaya.Hedgehog.Fold (embeddableOfHeight)

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

expression :: (Steppable (->) t Expr) => Size -> Gen t
expression = embeddableOfHeight genExprLit genExpr

genMuExpr :: Size -> Gen (Mu Expr)
genMuExpr = expression

genNuExpr :: Size -> Gen (Nu Expr)
genNuExpr = expression

genFixExpr :: Size -> Gen (Fix Expr)
genFixExpr = expression

genCofixExpr :: Size -> Gen (Cofix Expr)
genCofixExpr = expression
