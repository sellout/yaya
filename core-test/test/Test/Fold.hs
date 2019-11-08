{-# LANGUAGE TemplateHaskell #-}

module Test.Fold where

import           Data.Proxy
import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya.Fold
import           Yaya.Fold.Common
import           Yaya.Hedgehog.Expr
import           Yaya.Hedgehog.Fold

prop_muCataCancel :: Property
prop_muCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genMuExpr))

prop_muCataRefl :: Property
prop_muCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genMuExpr)

prop_muCataCompose :: Property
prop_muCataCompose =
  property
  $ law_cataCompose (Proxy :: Proxy (Mu Expr)) size id
    =<< forAll (Gen.sized genMuExpr)

tests :: IO Bool
tests = checkParallel $$(discover)
