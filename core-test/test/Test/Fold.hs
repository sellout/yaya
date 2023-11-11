{-# LANGUAGE TemplateHaskell #-}

module Test.Fold where

import Control.Category (Category (..))
import Control.Monad ((=<<))
import Data.Bool (Bool)
import Data.Function (($))
import Data.Proxy (Proxy (..))
import Hedgehog (Property, checkParallel, discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import System.IO (IO)
import Yaya.Fold (Mu)
import Yaya.Fold.Common (size)
import Yaya.Hedgehog.Expr (Expr, genExpr, genMuExpr)
import Yaya.Hedgehog.Fold (law_cataCancel, law_cataCompose, law_cataRefl)

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
