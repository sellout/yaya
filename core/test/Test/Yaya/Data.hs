{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Data where

import           Hedgehog

import           Yaya
import           Yaya.Data

import           Test.Expr
import           Test.Yaya.Control

genMuExpr :: Gen (Mu Expr)
genMuExpr = genEmbeddable genExpr

genNuExpr :: Gen (Nu Expr)
genNuExpr = genEmbeddable genExpr

prop_muCataCancel :: Property
prop_muCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr genMuExpr)

prop_muCataRefl :: Property
prop_muCataRefl =
  property $ law_cataRefl =<< forAll genMuExpr

-- prop_muCataCompose :: Property
-- prop_muCataCompose =
--   property $ law_cataCompose size id =<< forAll genMuExpr

tests :: IO Bool
tests = checkParallel $$(discover)
