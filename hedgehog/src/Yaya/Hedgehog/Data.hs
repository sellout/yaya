module Yaya.Hedgehog.Data where

import           Hedgehog

import           Yaya
import           Yaya.Data

import           Yaya.Hedgehog.Control
import           Yaya.Hedgehog.Expr

genMuExpr :: Gen (Mu Expr)
genMuExpr = genEmbeddable genExpr

genNuExpr :: Gen (Nu Expr)
genNuExpr = genEmbeddable genExpr
