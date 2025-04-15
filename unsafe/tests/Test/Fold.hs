{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold (tests) where

import safe "base" Control.Category (id, (.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import safe "hedgehog" Hedgehog
  ( Property,
    checkParallel,
    discover,
    forAll,
    property,
    withTests,
    (===),
  )
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold
  ( Corecursive,
    Mu,
    Nu,
    Projectable,
    Recursive,
    ana,
    cata,
    project,
  )
import safe "yaya" Yaya.Fold.Common (size)
import safe "yaya" Yaya.Fold.Native (Cofix)
import safe "yaya" Yaya.Pattern (Pair ((:!:)), fst)
import "yaya-hedgehog" Yaya.Hedgehog (evalNonterminating)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr
  ( Expr,
    genCofixExpr,
    genExpr,
    -- genFixExpr,
    -- genMuExpr,
    -- genNuExpr,
  )
import safe "yaya-hedgehog" Yaya.Hedgehog.Fold
  ( -- law_anaRefl,
    law_cataCancel,
    law_cataCompose,
    law_cataRefl,
  )
import "yaya-unsafe" Yaya.Unsafe.Fold (Unsafe (Unsafe))

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- prop_fixAnaRefl :: Property
-- prop_fixAnaRefl =
--   property $ law_anaRefl =<< forAll (fmap Unsafe . Gen.sized genFixExpr)

-- -- | NB: Only in yaya-unsafe instead of yaya because the `Eq (Cofix f)` instance
-- --       is needed.
-- prop_cofixAnaRefl :: Property
-- prop_cofixAnaRefl =
--   property $ law_anaRefl =<< forAll (fmap Unsafe . (Gen.sized genCofixExpr))

prop_cofixCataCancel :: Property
prop_cofixCataCancel =
  property $
    law_cataCancel size
      =<< forAll (genExpr (Unsafe <$> Gen.sized genCofixExpr))

prop_cofixCataRefl :: Property
prop_cofixCataRefl =
  property $ law_cataRefl =<< forAll (Unsafe <$> Gen.sized genCofixExpr)

prop_cofixCataCompose :: Property
prop_cofixCataCompose =
  property $
    law_cataCompose (Proxy :: Proxy (Unsafe (Cofix Expr))) size id
      =<< forAll (Unsafe <$> Gen.sized genCofixExpr)

-- -- | NB: Only in yaya-unsafe instead of yaya because the `Eq (Nu f)` instance is
-- --       needed.
-- prop_nuAnaRefl :: Property
-- prop_nuAnaRefl =
--   property $ law_anaRefl =<< forAll (Gen.sized genNuExpr)

-- prop_nuCataCancel :: Property
-- prop_nuCataCancel =
--   property $
--     law_cataCancel size =<< forAll (genExpr (Unsafe <$> Gen.sized genNuExpr))

-- prop_nuCataRefl :: Property
-- prop_nuCataRefl =
--   property $ law_cataRefl =<< forAll (Unsafe <$> Gen.sized genNuExpr)

-- prop_nuCataCompose :: Property
-- prop_nuCataCompose =
--   property $
--     law_cataCompose (Proxy :: Proxy (Unsafe (Nu Expr))) size id
--       =<< forAll (Unsafe <$> Gen.sized genNuExpr)

-- prop_muAnaRefl :: Property
-- prop_muAnaRefl =
--   property $ law_anaRefl =<< forAll (Gen.sized genMuExpr)

-- * These tests try to verify non-termination behavior.

-- | Show that using a `Recursive` structure corecursively can lead to
--   non-termination.
corecursiveIsUnsafe ::
  forall t a.
  ( Corecursive (->) (Unsafe (t (Pair a))) (Pair a),
    Projectable (->) (Unsafe (t (Pair a))) (Pair a),
    -- Corecursive (->) (Unsafe (t ((,) a))) ((,) a),
    -- Projectable (->) (Unsafe (t ((,) a))) ((,) a),
    -- Eq a,
    Show a
  ) =>
  Proxy t ->
  a ->
  Property
corecursiveIsUnsafe Proxy x =
  withTests 1 . property $ do
    -- a properly-finite data structure will diverge on infinite unfolding
    evalNonterminating . fst . project @_ @(Unsafe (t (Pair a))) $ ana (\y -> y :!: y) x

-- -- but using a lazy functor loses this property
-- Tuple.fst (project @_ @(Unsafe (t ((,) a))) $ ana (\y -> (y, y)) x) === x

-- | Show that using a `Corecursive` structure recursively can lead to
--   non-termination.
recursiveIsUnsafe ::
  forall t a.
  ( Corecursive (->) (t (Pair a)) (Pair a),
    Projectable (->) (t (Pair a)) (Pair a),
    Recursive (->) (Unsafe (t (Pair a))) (Pair a),
    -- Corecursive (->) (t ((,) a)) ((,) a),
    -- Recursive (->) (Unsafe (t ((,) a))) ((,) a),
    Eq a,
    Show a
  ) =>
  Proxy t ->
  a ->
  Property
recursiveIsUnsafe Proxy x =
  withTests 1 . property $ do
    -- We can easily get the first element of a corecursive infinite sequence
    fst (project $ ana @_ @(t (Pair a)) (\y -> y :!: y) x) === x
    -- Of course, you can’t fold it.
    evalNonterminating . cata fst . Unsafe $ ana @_ @(t (Pair a)) (\y -> y :!: y) x

-- -- But again, if you use a lazy functor, you lose that property, and you can
-- -- short-circuit.
-- cata Tuple.fst (Unsafe $ ana @_ @(t ((,) a)) (\y -> (y, y)) x) === x

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
