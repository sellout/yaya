{-# LANGUAGE TemplateHaskell #-}

module Test.Fold.Native where

import "base" Control.Category (Category (id))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Proxy (Proxy (Proxy))
import "base" System.IO (IO)
import "hedgehog" Hedgehog (Property, checkParallel, discover, forAll, property)
import qualified "hedgehog" Hedgehog.Gen as Gen
import "yaya" Yaya.Fold.Common (size)
import "yaya" Yaya.Fold.Native (Fix)
import "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, genExpr, genFixExpr)
import "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

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
tests = checkParallel $$discover
