{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Fold where

import Data.Proxy
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Yaya.Fold
import Yaya.Fold.Common
import Yaya.Fold.Native
import Yaya.Hedgehog.Expr
import Yaya.Hedgehog.Fold
import qualified Yaya.Unsafe.Fold.Instances ()

prop_fixAnaRefl :: Property
prop_fixAnaRefl =
  property $ law_anaRefl =<< forAll (Gen.sized genFixExpr)

-- | NB: Only in yaya-unsafe instead of yaya because the `Eq (Cofix f)` instance
--       is needed.
prop_cofixAnaRefl :: Property
prop_cofixAnaRefl =
  property $ law_anaRefl =<< forAll (Gen.sized genCofixExpr)

prop_cofixCataCancel :: Property
prop_cofixCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genCofixExpr))

prop_cofixCataRefl :: Property
prop_cofixCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genCofixExpr)

prop_cofixCataCompose :: Property
prop_cofixCataCompose =
  property $
    law_cataCompose (Proxy :: Proxy (Fix Expr)) size id
      =<< forAll (Gen.sized genCofixExpr)

-- | NB: Only in yaya-unsafe instead of yaya because the `Eq (Nu f)` instance is
--       needed.
prop_nuAnaRefl :: Property
prop_nuAnaRefl =
  property $ law_anaRefl =<< forAll (Gen.sized genNuExpr)

prop_nuCataCancel :: Property
prop_nuCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genNuExpr))

prop_nuCataRefl :: Property
prop_nuCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genNuExpr)

prop_nuCataCompose :: Property
prop_nuCataCompose =
  property $
    law_cataCompose (Proxy :: Proxy (Nu Expr)) size id
      =<< forAll (Gen.sized genNuExpr)

prop_muAnaRefl :: Property
prop_muAnaRefl =
  property $ law_anaRefl =<< forAll (Gen.sized genMuExpr)

-- * These tests try to verify non-termination behavior.

prop_muIsntCorecursive :: Property
prop_muIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Mu) (1 :: Int)

prop_nuIsntRecursive :: Property
prop_nuIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Nu) (1 :: Int)

prop_fixIsntCorecursive :: Property
prop_fixIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Fix) (1 :: Int)

prop_cofixIsntRecursive :: Property
prop_cofixIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Cofix) (1 :: Int)

tests :: IO Bool
tests = checkParallel $$(discover)
