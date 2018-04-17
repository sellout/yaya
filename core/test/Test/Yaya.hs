{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya where

import           Hedgehog

import           Yaya
import           Yaya.Control
import           Yaya.Hedgehog.Data

prop_heightLtSize :: Property
prop_heightLtSize =
  property $ do
    expr <- forAll genMuExpr
    -- replace with
    --     . uncurry (<) $ cata (height `zip` size) expr
    assert $ cata height expr < fromIntegral (cata size expr)

tests :: IO Bool
tests = checkParallel $$(discover)
