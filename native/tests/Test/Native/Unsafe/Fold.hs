{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Native.Unsafe.Fold (tests) where

import safe "base" Control.Category (id)
import safe "base" Data.Bool (Bool)
import safe "base" Data.Int (Int)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu)
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr)
import safe qualified "yaya-hedgehog" Yaya.Hedgehog.Expr as Expr
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( -- corecursiveIsUnsafe,
    recursiveIsUnsafe,
  )
import safe "yaya-native" Yaya.Native.Fold (Cofix, Fix)
import safe qualified "yaya-native" Yaya.Native.Unsafe.Fold.Instances ()

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

-- TODO: Figure out why this hangs during compilation.
-- prop_fixIsntCorecursive :: Property
-- prop_fixIsntCorecursive = corecursiveIsUnsafe (Proxy :: Proxy Fix) (1 :: Int)

prop_cofixIsntRecursive :: Property
prop_cofixIsntRecursive = recursiveIsUnsafe (Proxy :: Proxy Cofix) (1 :: Int)

tests :: IO Bool
tests = checkParallel $$discover
