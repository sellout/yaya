{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya where

import           Hedgehog

import           Yaya
import           Yaya.Control

import           Test.Yaya.Data

-- | Allows us to create an arbitrarily deep instance of any Embeddable type.
genEmbeddable :: Embeddable t f => (forall a. Gen a -> Gen (f a)) -> Gen t
genEmbeddable f = embed <$> f (genEmbeddable f)

genCorecursive :: Corecursive t f => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana

prop_heightLtSize :: Property
prop_heightLtSize =
  property $ do
    expr <- forAll genMuExpr
    -- replace with
    --     . uncurry (<) $ cata (height `zip` size) expr
    assert $ cata height expr < fromIntegral (cata size expr)

law_cataRefl :: (Eq t, Show t, Embeddable t f, Recursive t f, MonadTest m) => t -> m ()
law_cataRefl t = cata embed t === t

prop_muCataRefl :: Property
prop_muCataRefl =
  property $ law_cataRefl =<< forAll genMuExpr
    

tests :: IO Bool
tests = checkParallel $$(discover)
