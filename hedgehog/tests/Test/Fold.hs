{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold (tests) where

import safe "base" Control.Category (id)
import safe "base" Data.Bool (Bool)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu)
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr)
import safe qualified "yaya-hedgehog" Yaya.Hedgehog.Expr as Expr

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_muCataCancel :: Property
prop_muCataCancel =
  Expr.cataCancel (Proxy :: Proxy (Mu Expr)) size Gen.word8 0 20

prop_muCataRefl :: Property
prop_muCataRefl = Expr.cataRefl (Proxy :: Proxy (Mu Expr)) Gen.word8 0 20

prop_muCataCompose :: Property
prop_muCataCompose =
  Expr.cataCompose
    (Proxy :: Proxy (Mu Expr))
    (Proxy :: Proxy (Mu Expr))
    size
    id
    Gen.word8
    0
    20

tests :: IO Bool
tests = checkParallel $$discover
