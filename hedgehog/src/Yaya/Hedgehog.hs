{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog where

import           Hedgehog

import           Yaya
import           Yaya.Control

-- | Allows us to create an arbitrarily deep instance of any Embeddable type.
genEmbeddable :: Embeddable t f => (forall a. Gen a -> Gen (f a)) -> Gen t
genEmbeddable f = embed <$> f (genEmbeddable f)

genCorecursive :: Corecursive t f => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana
