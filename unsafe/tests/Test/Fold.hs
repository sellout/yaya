{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold (tests) where

import safe "base" Control.Category (id)
import safe "base" Data.Bool (Bool)
import safe "base" Data.Int (Int)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu, Nu)
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya" Yaya.Fold.Native (Cofix, Fix)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr)
import safe qualified "yaya-hedgehog" Yaya.Hedgehog.Expr as Expr
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( corecursiveIsUnsafe,
    recursiveIsUnsafe,
  )
import safe qualified "yaya-unsafe" Yaya.Unsafe.Fold.Instances ()

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_fixAnaRefl :: Property
prop_fixAnaRefl = Expr.anaRefl (Proxy :: Proxy (Fix Expr)) Gen.word8 0 20

-- |
--
--  __NB__: Only in yaya-unsafe instead of yaya because the `Eq (Cofix f)`
--          instance is needed.
prop_cofixAnaRefl :: Property
prop_cofixAnaRefl = Expr.anaRefl (Proxy :: Proxy (Cofix Expr)) Gen.word8 0 20

prop_cofixCataCancel :: Property
prop_cofixCataCancel =
  Expr.cataCancel (Proxy :: Proxy (Mu Expr)) size Gen.word8 0 20

prop_cofixCataRefl :: Property
prop_cofixCataRefl = Expr.cataRefl (Proxy :: Proxy (Cofix Expr)) Gen.word8 0 20

prop_cofixCataCompose :: Property
prop_cofixCataCompose =
  Expr.cataCompose
    (Proxy :: Proxy (Cofix Expr))
    (Proxy :: Proxy (Cofix Expr))
    size
    id
    Gen.word8
    0
    20

-- |
--
--  __NB__: Only in yaya-unsafe instead of yaya because the `Eq (Nu f)` instance
--          is needed.
prop_nuAnaRefl :: Property
prop_nuAnaRefl = Expr.anaRefl (Proxy :: Proxy (Nu Expr)) Gen.word8 0 20

prop_nuCataCancel :: Property
prop_nuCataCancel =
  Expr.cataCancel (Proxy :: Proxy (Nu Expr)) size Gen.word8 0 20

prop_nuCataRefl :: Property
prop_nuCataRefl = Expr.cataRefl (Proxy :: Proxy (Nu Expr)) Gen.word8 0 20

prop_nuCataCompose :: Property
prop_nuCataCompose =
  Expr.cataCompose
    (Proxy :: Proxy (Nu Expr))
    (Proxy :: Proxy (Nu Expr))
    size
    id
    Gen.word8
    0
    20

prop_muAnaRefl :: Property
prop_muAnaRefl = Expr.anaRefl (Proxy :: Proxy (Mu Expr)) Gen.word8 0 20

-- * These tests try to verify non-termination behavior.

prop_muIsntCorecursive :: Property
prop_muIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Mu) (1 :: Int)

prop_nuIsntRecursive :: Property
prop_nuIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Nu) (1 :: Int)

-- TODO: Figure out why this hangs during compilation.
-- prop_fixIsntCorecursive :: Property
-- prop_fixIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Fix) (1 :: Int)

prop_cofixIsntRecursive :: Property
prop_cofixIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Cofix) (1 :: Int)

tests :: IO Bool
tests = checkParallel $$discover
