{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold.Native where

import safe "base" Control.Category (Category (id))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Function (($))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover, forAll, property)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya" Yaya.Fold.Native (Fix)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, genExpr, genFixExpr)
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
