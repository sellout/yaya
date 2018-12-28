{-# LANGUAGE TemplateHaskell #-}

module Test.Fold where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya.Fold.Common
import           Yaya.Hedgehog.Expr
import           Yaya.Hedgehog.Fold
import           Yaya.Unsafe.Fold.Instances ()

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
