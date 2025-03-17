{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold (tests) where

import safe "base" Control.Category (id, (.))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Functor (fmap)
import safe "base" Data.Int (Int)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Word (Word8)
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog (Property, checkParallel, discover)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu, Nu, Recursive, cata, project)
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya" Yaya.Pattern (Maybe)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr)
import safe qualified "yaya-hedgehog" Yaya.Hedgehog.Expr as Expr
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( corecursiveIsUnsafe,
    recursiveIsUnsafe,
  )
import safe qualified "yaya-unsafe" Yaya.Unsafe.Fold.Instances ()

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
--
--  __FIXME__: Remove this instance, since we already have one downstream.
instance Recursive (->) Word8 Maybe where
  cata φ = φ . fmap (cata φ) . project

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

tests :: IO Bool
tests = checkParallel $$discover
