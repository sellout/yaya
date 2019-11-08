{-# LANGUAGE TemplateHaskell #-}

module Test.Fold.Common where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya.Fold
import           Yaya.Fold.Common
import           Yaya.Hedgehog.Expr

prop_heightLtSize :: Property
prop_heightLtSize =
  property
  (assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
   =<< forAll (Gen.sized genMuExpr))

tests :: IO Bool
tests = checkParallel $$(discover)
