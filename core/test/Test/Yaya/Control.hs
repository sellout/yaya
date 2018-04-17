{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Control where

import           Hedgehog

import           Yaya.Control

-- | Allows us to create an arbitrarily deep instance of any Embeddable type.
genEmbeddable :: Embeddable t f => (forall a. Gen a -> Gen (f a)) -> Gen t
genEmbeddable f = embed <$> f (genEmbeddable f)

genCorecursive :: Corecursive t f => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana

tests :: IO Bool
tests = checkParallel $$(discover)
