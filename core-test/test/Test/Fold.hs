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
import "yaya" Yaya.Fold (Mu)
import "yaya" Yaya.Fold.Common (size)
import "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, genExpr, genMuExpr)
import "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )

prop_muCataCancel :: Property
prop_muCataCancel =
  property $ law_cataCancel size =<< forAll (genExpr (Gen.sized genMuExpr))

prop_muCataRefl :: Property
prop_muCataRefl =
  property $ law_cataRefl =<< forAll (Gen.sized genMuExpr)

prop_muCataCompose :: Property
prop_muCataCompose =
  property $
    law_cataCompose (Proxy :: Proxy (Mu Expr)) size id
      =<< forAll (Gen.sized genMuExpr)

tests :: IO Bool
tests = checkParallel $$(discover)
