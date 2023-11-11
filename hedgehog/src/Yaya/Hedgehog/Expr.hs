{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog.Expr where

import Control.Applicative (Applicative (..))
import Data.Eq (Eq)
import Data.Eq.Deriving (deriveEq1)
import Data.Foldable (Foldable)
import Data.Functor (Functor, (<$>))
import Data.Int (Int)
import Data.Traversable (Traversable)
import Hedgehog (Gen, Size)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Show (Show)
import Text.Show.Deriving (deriveShow1)
import Yaya.Fold (Mu, Nu, Steppable)
import Yaya.Fold.Native (Fix)
import Yaya.Hedgehog.Fold (embeddableOfHeight)

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
