{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Yaya.Hedgehog.Fold where

import safe "base" Control.Category (Category (..))
import safe "base" Data.Bifunctor (Bifunctor (..))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (..))
import safe "base" Data.Proxy (Proxy (..))
import safe qualified "base" Data.Tuple as Tuple
import safe "base" Data.Void (Void, absurd)
import safe "base" Numeric.Natural (Natural)
import safe "base" Text.Show (Show)
import "hedgehog" Hedgehog
  ( Gen,
    MonadTest,
    Property,
    Size,
    property,
    withTests,
    (===),
  )
import "yaya" Yaya.Fold
  ( Algebra,
    Corecursive (..),
    Projectable (..),
    Recursive (..),
    Steppable (..),
  )
import safe "yaya" Yaya.Fold.Common (diagonal)
import safe "yaya" Yaya.Fold.Native ()
import safe "yaya" Yaya.Pattern (Maybe, Pair ((:!:)), fst, maybe, uncurry)
import "this" Yaya.Hedgehog (evalNonterminating)
import safe "base" Prelude (fromIntegral)

{-# HLINT ignore "Use camelCase" #-}

law_cataCancel ::
  ( Eq a,
    Show a,
    Steppable (->) t f,
    Recursive (->) t f,
    Functor f,
    MonadTest m
  ) =>
  Algebra (->) f a ->
  f t ->
  m ()
law_cataCancel φ =
  uncurry (===) . bimap (cata φ . embed) (φ . fmap (cata φ)) . diagonal

law_cataRefl ::
  (Eq t, Show t, Steppable (->) t f, Recursive (->) t f, MonadTest m) =>
  t ->
  m ()
law_cataRefl = uncurry (===) . first (cata embed) . diagonal

-- | NB: Since this requires both a `Corecursive` and `Eq` instance on the same
--       type, it _likely_ requires instances from yaya-unsafe.
law_anaRefl ::
  (Eq t, Show t, Steppable (->) t f, Corecursive (->) t f, MonadTest m) =>
  t ->
  m ()
law_anaRefl = uncurry (===) . first (ana project) . diagonal

-- law_cataFusion ::
--   (Eq a, Show a, Recursive (->) t f, Functor f, MonadTest m) =>
--   (a -> a) ->
--   Algebra (->) f a ->
--   f a ->
--   t ->
--   m ()
-- law_cataFusion f φ fa t =
--   uncurry (==) (bimap (f . φ) (φ . fmap f) $ diagonal fa)
--     ==> uncurry (===) (bimap (f . cata φ) (cata φ) $ diagonal t)

law_cataCompose ::
  forall t f u g m b.
  ( Eq b,
    Show b,
    Recursive (->) t f,
    Steppable (->) u g,
    Recursive (->) u g,
    MonadTest m
  ) =>
  Proxy u ->
  Algebra (->) g b ->
  (forall a. f a -> g a) ->
  t ->
  m ()
law_cataCompose Proxy φ ε =
  uncurry (===)
    . bimap (cata φ . cata (embed . ε :: f u -> u)) (cata (φ . ε))
    . diagonal

-- | Creates a generator for any `Steppable` type whose pattern functor has
--   terminal cases (e.g., not `Data.Functor.Identity` or `((,) a)`). @leaf@ can
--   only generate terminal cases, and `branch` can generate any case. If the
--   provided `branch` generates terminal cases, then the resulting tree may
--   have a height less than the `Size`, otherwise it will be a perfect tree
--   with a height of exactly the provided `Size`.
--
--   This is similar to `Gen.recursive` in that it separates the non-recursive
--   cases from the recursive ones, except
--
-- * the types here also ensure that the non-recursive cases aren’t recursive,
--
-- * different generator distributions may be used for rec & non-rec cases, and
--
-- * the non-recursive cases aren’t included in recursive calls (see above for
--   why).
--
--   If there’s no existing @Gen (f Void)@ for your pattern functor, you can
--   either create one manually, or pass `Hedgehog.Gen.discard` to the usual
--  @Gen a -> Gen (f a)@ generator.
--
--  NB: Hedgehog’s `Size` is signed, so this can raise an exception if given a
--      negative `Size`.
embeddableOfHeight ::
  (Steppable (->) t f, Functor f) =>
  Gen (f Void) ->
  (Gen t -> Gen (f t)) ->
  Size ->
  Gen t
embeddableOfHeight leaf branch size =
  cata (genAlgebra leaf branch) (fromIntegral size :: Natural)

-- | Builds a generic tree generator of a certain height.
genAlgebra ::
  (Steppable (->) t f, Functor f) =>
  Gen (f Void) ->
  (Gen t -> Gen (f t)) ->
  Algebra (->) Maybe (Gen t)
genAlgebra leaf branch =
  maybe (fmap (embed . fmap absurd) leaf) (fmap embed . branch)

-- | Creates a generator for potentially-infinite values.
genCorecursive :: (Corecursive (->) t f) => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana

-- | Show that using a `Recursive` structure corecursively can lead to
--   non-termination.
corecursiveIsUnsafe ::
  forall t a.
  ( Corecursive (->) (t (Pair a)) (Pair a),
    Projectable (->) (t (Pair a)) (Pair a),
    Corecursive (->) (t ((,) a)) ((,) a),
    Projectable (->) (t ((,) a)) ((,) a),
    Eq a,
    Show a
  ) =>
  Proxy t ->
  a ->
  Property
corecursiveIsUnsafe Proxy x =
  withTests 1 . property $ do
    -- a properly-finite data structure will diverge on infinite unfolding
    evalNonterminating . fst . project @_ @(t (Pair a)) $ ana (\y -> y :!: y) x
    -- but using a lazy functor loses this property
    Tuple.fst (project @_ @(t ((,) a)) $ ana (\y -> (y, y)) x) === x

-- | Show that using a `Corecursive` structure recursively can lead to
--   non-termination.
recursiveIsUnsafe ::
  forall t a.
  ( Corecursive (->) (t (Pair a)) (Pair a),
    Projectable (->) (t (Pair a)) (Pair a),
    Recursive (->) (t (Pair a)) (Pair a),
    Corecursive (->) (t ((,) a)) ((,) a),
    Recursive (->) (t ((,) a)) ((,) a),
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
    evalNonterminating . cata fst $ ana @_ @(t (Pair a)) (\y -> y :!: y) x
    -- But again, if you use a lazy functor, you lose that property, and you can
    -- short-circuit.
    cata Tuple.fst (ana @_ @(t ((,) a)) (\y -> (y, y)) x) === x
