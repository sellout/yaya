{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaya.Hedgehog.Control where

import           Control.Arrow
import           Data.Void
import           Hedgehog
import           Numeric.Natural

import           Yaya
import           Yaya.Control
import           Yaya.Data

law_cataCancel
  :: (Eq a, Show a, Steppable t f, Recursive t f, Functor f, MonadTest m)
  => Algebra f a -> f t -> m ()
law_cataCancel φ = uncurry (===) . (cata φ . embed &&& φ . fmap (cata φ))

law_cataRefl
  :: (Eq t, Show t, Steppable t f, Recursive t f, MonadTest m) => t -> m ()
law_cataRefl = uncurry (===) . (cata embed &&& id)

-- law_cataFusion
--   :: (Eq a, Show a, Recursive t f, Functor f, MonadTest m)
--   => (a -> a) -> Algebra f a -> f a -> t -> m ()
-- law_cataFusion f φ fa t =
--       uncurry (==) ((f . φ &&& φ . fmap f) fa)
--   ==> uncurry (===) ((f . cata φ &&& cata φ) t)

-- law_cataCompose
--   :: (Eq b, Show b, Recursive t f, Steppable u g, Recursive u g, MonadTest m)
--   => Algebra g b -> (forall a. f a -> g a) -> t -> m ()
-- law_cataCompose φ ε =
--   uncurry (===) . (cata φ . cata (embed . ε) &&& cata (φ . ε))

-- | Creates a generator for any `Steppable` type whose pattern functor has
--   terminal cases (e.g., not `Identity` or `((,) a)`). `leaf` can only
--   generate terminal cases, and `any` can generate any case. If the provided
--  `any` generates terminal cases, then the resulting tree may have a height
--   less than the `Size`, otherwise it will be a perfect tree with a height of
--   exactly the provided `Size`.
--
--   This is similar to `Gen.recursive` in that it separates the non-recursive
--   cases from the recursive ones, except
-- • the types here also ensure that the non-recursive cases aren’t recursive,
-- • different generator distributions may be used for rec & non-rec cases, and
-- • the non-recursive cases aren’t included in recursive calls (see above for
--   why).
--
--   If there’s no existing `Gen (f Void)` for your pattern functor, you can
--   either create one manually, or pass `Hedgehog.Gen.discard` to the usual
--  `Gen a -> Gen (f a)` generator.
--
--  *NB*: Hedgehog’s `Size` is signed, so this can raise an exception if given a
--        negative `Size`.
embeddableOfHeight
  :: (Steppable t f, Functor f)
  => Gen (f Void) -> (Gen t -> Gen (f t)) -> Size -> Gen t
embeddableOfHeight leaf any size =
  cata (genAlgebra leaf any) (fromIntegral size :: Natural)

-- | Builds a generic tree generator of a certain height.
genAlgebra
  :: (Steppable t f, Functor f)
  => Gen (f Void) -> (Gen t -> Gen (f t)) -> Algebra Maybe (Gen t)
genAlgebra leaf any = maybe (fmap (embed . fmap absurd) leaf) (fmap embed . any)

-- | Creates a generator for potentially-infinite values.
genCorecursive :: Corecursive t f => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana
