module Yaya.Hedgehog.Data where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya
import           Yaya.Control
import           Yaya.Data

import           Yaya.Hedgehog.Control
import           Yaya.Hedgehog.Expr

expression :: Steppable t Expr => Size -> Gen t
expression = embeddableOfHeight genExprLit genExpr

genMuExpr :: Size -> Gen (Mu Expr)
genMuExpr = expression

genNuExpr :: Size -> Gen (Nu Expr)
genNuExpr = expression
