{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Yaya
import           Yaya.Control
import           Yaya.Hedgehog.Data

prop_heightLtSize :: Property
prop_heightLtSize =
  property
  (assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
   =<< forAll (Gen.sized genMuExpr))

tests :: IO Bool
tests = checkParallel $$(discover)
