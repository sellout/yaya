{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Hedgehog.Fold
  ( corecursiveIsUnsafe,
    embeddableOfHeight,
    genAlgebra,
    genCorecursive,
    law_anaRefl,
    law_cataCancel,
    law_cataCompose,
    law_cataRefl,
    recursiveIsUnsafe,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (bimap, first)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Proxy (Proxy (Proxy))
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
    Corecursive,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    embed,
    project,
  )
import safe "yaya" Yaya.Fold.Common (diagonal)
import safe "yaya" Yaya.Fold.Native ()
import safe "yaya" Yaya.Pattern (Maybe, Pair ((:!:)), fst, maybe, uncurry)
import "this" Yaya.Hedgehog (evalNonterminating)
import safe "base" Prelude (fromIntegral)

{-# HLINT ignore "Use camelCase" #-}

-- | The cancellation law of `cata` – folding a freshly embedded layer is the
--   same as applying the algebra to that layer with its children folded.
--
-- @since 0.1.0
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

-- | The reflection law of `cata` – folding with `embed` itself is the identity.
--
-- @since 0.1.0
law_cataRefl ::
  (Eq t, Show t, Steppable (->) t f, Recursive (->) t f, MonadTest m) =>
  t ->
  m ()
law_cataRefl = uncurry (===) . first (cata embed) . diagonal

-- | NB: Since this requires both a `Corecursive` and `Eq` instance on the same
--       type, it /likely/ requires instances from yaya-unsafe.
--
-- @since 0.1.1
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

-- | The composition law of `cata`. This follows from fusion and the naturality
--   of @ε@. The `Proxy` selects the intermediate fixed point, @u@, of @g@.
--
-- @since 0.1.2
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
{-# INLINEABLE law_cataCompose #-}

-- | Creates a generator for any `Steppable` type whose pattern functor has
--   terminal cases (e.g., not `Data.Functor.Identity` or `((,) a)`). @leaf@ can
--   only generate terminal cases, and @branch@ can generate any case.
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
--   If there’s no existing @`Gen` (f `Void`)@ for your pattern functor, you can
--   either create one manually, or pass `Hedgehog.Gen.discard` to the usual
--   @`Gen` a -> `Gen` (f a)@ generator.
--
--  __NB__: Hedgehog’s `Size` is signed, so this can raise an exception if given
--          a negative `Size`.
--
-- @since 0.1.0
embeddableOfHeight ::
  (Steppable (->) t f, Functor f) =>
  -- | A generator for terminal cases (leaf nodes).
  Gen (f Void) ->
  -- | A generator for arbitrary cases. If the provided value generates terminal
  --   cases, then the resulting tree may have a height less than the `Size`,
  --   otherwise it will be a perfect tree with a height of exactly the provided
  --  `Size`.
  (Gen t -> Gen (f t)) ->
  Size ->
  Gen t
embeddableOfHeight leaf branch size =
  cata (genAlgebra leaf branch) (fromIntegral size :: Natural)

-- | Builds a generic tree generator of a certain height.
--
-- @since 0.1.0
genAlgebra ::
  (Steppable (->) t f, Functor f) =>
  Gen (f Void) ->
  (Gen t -> Gen (f t)) ->
  Algebra (->) Maybe (Gen t)
genAlgebra leaf branch =
  maybe (fmap (embed . fmap absurd) leaf) (fmap embed . branch)
{-# INLINEABLE genAlgebra #-}

-- | Creates a generator for potentially-infinite values.
--
-- @since 0.1.0
genCorecursive :: (Corecursive (->) t f) => (a -> f a) -> Gen a -> Gen t
genCorecursive = fmap . ana

-- | Show that using a `Recursive` structure corecursively can lead to
--   non-termination.
--
-- @since 0.2.1
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
    evalNonterminating . fst . project @_ @_ @(t (Pair a)) $ ana (\y -> y :!: y) x
    -- but using a lazy functor loses this property
    Tuple.fst (project @_ @_ @(t ((,) a)) $ ana (\y -> (y, y)) x) === x
{-# INLINEABLE corecursiveIsUnsafe #-}

-- | Show that using a `Corecursive` structure recursively can lead to
--   non-termination.
--
-- @since 0.2.1
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
    fst (project $ ana @_ @_ @(t (Pair a)) (\y -> y :!: y) x) === x
    -- Of course, you can’t fold it.
    evalNonterminating . cata fst $ ana @_ @_ @(t (Pair a)) (\y -> y :!: y) x
    -- But again, if you use a lazy functor, you lose that property, and you can
    -- short-circuit.
    cata Tuple.fst (ana @_ @_ @(t ((,) a)) (\y -> (y, y)) x) === x
{-# INLINEABLE recursiveIsUnsafe #-}
