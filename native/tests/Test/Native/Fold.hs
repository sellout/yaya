{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Native.Fold (tests) where

import safe "base" Control.Category (id)
import safe "base" Data.Bool (Bool)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr)
import safe qualified "yaya-hedgehog" Yaya.Hedgehog.Expr as Expr
import safe "yaya-native" Yaya.Native.Fold (Fix)

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_fixCataCancel :: Property
prop_fixCataCancel =
  Expr.cataCancel (Proxy :: Proxy (Fix Expr)) size Gen.word8 0 20

prop_fixCataRefl :: Property
prop_fixCataRefl = Expr.cataRefl (Proxy :: Proxy (Fix Expr)) Gen.word8 0 20

prop_fixCataCompose :: Property
prop_fixCataCompose =
  Expr.cataCompose
    (Proxy :: Proxy (Fix Expr))
    (Proxy :: Proxy (Fix Expr))
    size
    id
    Gen.word8
    0
    20

tests :: IO Bool
tests = checkParallel $$discover
