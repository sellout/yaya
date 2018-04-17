{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Unsafe.Data where

import           Hedgehog

import           Yaya
import           Yaya.Hedgehog.Control
import           Yaya.Hedgehog.Data
import           Yaya.Hedgehog.Expr
import           Yaya.Unsafe.Data

-- TODO: Should we have a yaya-native-hedgehog package too?
genFixExpr :: Gen (Fix Expr)
genFixExpr = genEmbeddable genExpr

prop_fixCataCancel :: Property
prop_fixCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr genFixExpr)

prop_fixCataRefl :: Property
prop_fixCataRefl =
  property $ law_cataRefl =<< forAll genFixExpr

prop_nuCataCancel :: Property
prop_nuCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr genNuExpr)

prop_nuCataRefl :: Property
prop_nuCataRefl =
  property $ law_cataRefl =<< forAll genNuExpr

-- prop_nuCataCompose :: Property
-- prop_nuCataCompose =
--   property $ law_cataCompose size id =<< forAll genNuExpr

tests :: IO Bool
tests = checkParallel $$(discover)
