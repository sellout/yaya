{-# LANGUAGE TemplateHaskell #-}

module Test.Fold.Common where

import "base" Control.Category (Category (..))
import "base" Control.Monad ((=<<))
import "base" Data.Bool (Bool)
import "base" Data.Functor (Functor (..))
import "base" Data.Ord (Ord (..))
import "hedgehog" Hedgehog
  ( Property,
    assert,
    checkParallel,
    discover,
    forAll,
    property,
  )
import qualified "hedgehog" Hedgehog.Gen as Gen
import "base" System.IO (IO)
import "yaya" Yaya.Fold (Recursive (..), zipAlgebras)
import "yaya" Yaya.Fold.Common (height, size)
import "yaya-hedgehog" Yaya.Hedgehog.Expr (genMuExpr)
import "yaya" Yaya.Pattern (uncurry)
import "base" Prelude (Integral (..))

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    ( assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
        =<< forAll (Gen.sized genMuExpr)
    )

tests :: IO Bool
tests = checkParallel $$(discover)
