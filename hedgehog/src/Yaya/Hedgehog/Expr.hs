{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog.Expr where

import "base" Control.Applicative (Applicative (..))
import "base" Data.Eq (Eq)
import "deriving-compat" Data.Eq.Deriving (deriveEq1)
import "base" Data.Foldable (Foldable)
import "base" Data.Functor (Functor, (<$>))
import "base" Data.Int (Int)
import "base" Data.Traversable (Traversable)
import "hedgehog" Hedgehog (Gen, Size)
import qualified "hedgehog" Hedgehog.Gen as Gen
import qualified "hedgehog" Hedgehog.Range as Range
import "base" Text.Show (Show)
import "deriving-compat" Text.Show.Deriving (deriveShow1)
import "yaya" Yaya.Fold (Mu, Nu, Steppable)
import "yaya" Yaya.Fold.Native (Fix)
import "this" Yaya.Hedgehog.Fold (embeddableOfHeight)

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
