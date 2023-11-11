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
import Yaya.Fold (Nu)
import Yaya.Fold.Common (size)
import Yaya.Fold.Native (Fix)
import Yaya.Hedgehog.Expr (Expr, genExpr, genFixExpr, genNuExpr)
import Yaya.Hedgehog.Fold (law_anaRefl, law_cataCancel, law_cataCompose, law_cataRefl)
import qualified Yaya.Unsafe.Fold.Instances ()

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
