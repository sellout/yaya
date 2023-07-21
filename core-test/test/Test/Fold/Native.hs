{-# LANGUAGE TemplateHaskell #-}

module Test.Fold.Native where

import Data.Proxy
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Yaya.Fold.Common
import Yaya.Fold.Native
import Yaya.Hedgehog.Expr
import Yaya.Hedgehog.Fold

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

tests :: IO Bool
tests = checkParallel $$(discover)
