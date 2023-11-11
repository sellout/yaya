{-# LANGUAGE TemplateHaskell #-}

module Test.Fold where

import "base" Control.Category (Category (..))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Proxy (Proxy (..))
import "hedgehog" Hedgehog (Property, checkParallel, discover, forAll, property)
import qualified "hedgehog" Hedgehog.Gen as Gen
import "base" System.IO (IO)
import "yaya" Yaya.Fold (Nu)
import "yaya" Yaya.Fold.Common (size)
import "yaya" Yaya.Fold.Native (Fix)
import "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, genExpr, genFixExpr, genNuExpr)
import "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( law_anaRefl,
    law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )
import qualified "yaya-unsafe" Yaya.Unsafe.Fold.Instances ()

-- | NB: Only in yaya-unsafe instead of yaya because the `Eq (Fix f)` instance
--       is needed.
prop_fixAnaRefl :: Property
prop_fixAnaRefl =
  property $ law_anaRefl =<< forAll (Gen.sized genFixExpr)

prop_fixCataCancel :: Property
prop_fixCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genFixExpr))

prop_fixCataRefl :: Property
prop_fixCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genFixExpr)

prop_fixCataCompose :: Property
prop_fixCataCompose =
  property $
    law_cataCompose (Proxy :: Proxy (Fix Expr)) size id
      =<< forAll (Gen.sized genFixExpr)

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

tests :: IO Bool
tests = checkParallel $$(discover)
