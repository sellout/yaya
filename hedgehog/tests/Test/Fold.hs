{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold (tests) where

import safe "base" Control.Category (id)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Function (($))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog
  ( Property,
    checkParallel,
    discover,
    forAll,
    property,
  )
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu)
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, genExpr, genMuExpr)
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
tests = checkParallel $$discover
