{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Fold where

import "base" Control.Category (Category (..))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Int (Int)
import "base" Data.Proxy (Proxy (..))
import "base" System.IO (IO)
import "hedgehog" Hedgehog (Property, checkParallel, discover, forAll, property)
import qualified "hedgehog" Hedgehog.Gen as Gen
import "yaya" Yaya.Fold (Mu, Nu)
import "yaya" Yaya.Fold.Common (size)
import "yaya" Yaya.Fold.Native (Cofix, Fix)
import "yaya-hedgehog" Yaya.Hedgehog.Expr
  ( Expr,
    genCofixExpr,
    genExpr,
    genFixExpr,
    genMuExpr,
    genNuExpr,
  )
import "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( corecursiveIsUnsafe,
    law_anaRefl,
    law_cataCancel,
    law_cataCompose,
    law_cataRefl,
    recursiveIsUnsafe,
  )
import qualified "yaya-unsafe" Yaya.Unsafe.Fold.Instances ()

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

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

-- TODO: Figure out why this hangs during compilation.
-- prop_fixIsntCorecursive :: Property
-- prop_fixIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Fix) (1 :: Int)

prop_cofixIsntRecursive :: Property
prop_cofixIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Cofix) (1 :: Int)

tests :: IO Bool
tests = checkParallel $$discover
