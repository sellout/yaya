{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Control where

import           Control.Arrow
import           Hedgehog

import           Yaya
import           Yaya.Control

law_cataCancel
  :: (Eq a, Show a, Embeddable t f, Recursive t f, Functor f, MonadTest m)
  => Algebra f a -> f t -> m ()
law_cataCancel φ = uncurry (===) . (cata φ . embed &&& φ . fmap (cata φ))

law_cataRefl
  :: (Eq t, Show t, Embeddable t f, Recursive t f, MonadTest m) => t -> m ()
law_cataRefl = uncurry (===) . (cata embed &&& id)

-- law_cataFusion
--   :: (Eq a, Show a, Recursive t f, Functor f, MonadTest m)
--   => (a -> a) -> Algebra f a -> f a -> t -> m ()
-- law_cataFusion f φ fa t =
--       uncurry (==) ((f . φ &&& φ . fmap f) fa)
--   ==> uncurry (===) ((f . cata φ &&& cata φ) t)

-- law_cataCompose
--   :: (Eq b, Show b, Recursive t f, Embeddable u g, Recursive u g, MonadTest m)
--   => Algebra g b -> (forall a. f a -> g a) -> t -> m ()
-- law_cataCompose φ ε =
--   uncurry (===) . (cata φ . cata (embed . ε) &&& cata (φ . ε))

-- | Allows us to create an arbitrarily deep instance of any Embeddable type.
genEmbeddable :: Embeddable t f => (forall a. Gen a -> Gen (f a)) -> Gen t
genEmbeddable f = embed <$> f (genEmbeddable f)

genCorecursive :: Corecursive t f => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana

tests :: IO Bool
tests = checkParallel $$(discover)
