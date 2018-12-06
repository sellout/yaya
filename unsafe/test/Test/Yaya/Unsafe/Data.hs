{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Unsafe.Data where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya
import           Yaya.Hedgehog.Control
import           Yaya.Hedgehog.Data
import           Yaya.Hedgehog.Expr
import           Yaya.Unsafe.Data

-- TODO: Should we have a yaya-native-hedgehog package too?
genFixExpr :: Size -> Gen (Fix Expr)
genFixExpr = expression

prop_fixCataCancel :: Property
prop_fixCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genFixExpr))

prop_fixCataRefl :: Property
prop_fixCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genFixExpr)

-- prop_fixCataCompose :: Property
-- prop_fixCataCompose =
--   property $ law_cataCompose size id =<< forAll (Gen.sized genFixExpr)

prop_nuCataCancel :: Property
prop_nuCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genNuExpr))

prop_nuCataRefl :: Property
prop_nuCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genNuExpr)

-- prop_nuCataCompose :: Property
-- prop_nuCataCompose =
--   property $ law_cataCompose size id =<< forAll (Gen.sized genNuExpr)

tests :: IO Bool
tests = checkParallel $$(discover)
