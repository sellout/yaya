{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Data where

import           Hedgehog

import           Yaya.Data

import           Test.Expr
import           Test.Yaya.Control

genMuExpr :: Gen (Mu Expr)
genMuExpr = genEmbeddable genExpr

genNuExpr :: Gen (Nu Expr)
genNuExpr = genEmbeddable genExpr

tests :: IO Bool
tests = checkParallel $$(discover)
