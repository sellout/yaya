{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Yaya.Hedgehog.Fold
  ( anaRefl,
    cataCancel,
    cataCompose,
    cataRefl,
    corecursiveIsUnsafe,
    genAlgebra,
    genCorecursive,
    law_anaRefl,
    law_cataCancel,
    law_cataCompose,
    law_cataRefl,
    recursiveIsUnsafe,
    steppableOfHeight,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe qualified "base" Data.Tuple as Tuple
import safe "base" Data.Void (Void, absurd)
import safe "base" Text.Show (Show)
import "hedgehog" Hedgehog
  ( Gen,
    MonadTest,
    Property,
    forAll,
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
import safe qualified "yaya" Yaya.Fold.Law as Law
import safe "yaya" Yaya.Pattern (Maybe, Pair ((:!:)), fst, maybe)
import "this" Yaya.Hedgehog (evalNonterminating)

#include "../../deprecation.h"

{-# HLINT ignore "Use camelCase" #-}

law_cataCancel ::
  ( Eq a,
    Show a,
    Recursive (->) t f,
    Steppable (->) t f,
    Functor f,
    MonadTest m
  ) =>
  Algebra (->) f a ->
  f t ->
  m ()
law_cataCancel = Law.cataCancel (===)
{- ORMOLU_DISABLE -}
DEPRECATE(law_cataCancel, "Use ‘cataCancel’ instead.", "yaya-hedgehog-0.4")
{- ORMOLU_ENABLE -}

cataCancel ::
  forall t a f.
  ( Eq a,
    Show a,
    Show (f t),
    Recursive (->) t f,
    Steppable (->) t f,
    Functor f
  ) =>
  Algebra (->) f a ->
  Gen (f t) ->
  Property
cataCancel φ = property . (Law.cataCancel (===) φ <=< forAll)
{-# INLINEABLE cataCancel #-}

law_cataRefl ::
  (Eq t, Show t, Steppable (->) t f, Recursive (->) t f, MonadTest m) =>
  t ->
  m ()
law_cataRefl = Law.cataRefl (===)
{- ORMOLU_DISABLE -}
DEPRECATE(law_cataRefl, "Use ‘cataRefl’ instead.", "yaya-hedgehog-0.4")
{- ORMOLU_ENABLE -}

cataRefl ::
  forall t f.
  (Eq t, Show t, Steppable (->) t f, Recursive (->) t f) =>
  Gen t ->
  Property
cataRefl = property . (Law.cataRefl (===) <=< forAll)
{-# INLINEABLE cataRefl #-}

-- | NB: Since this requires both a `Corecursive` and `Eq` instance on the same
--       type, it _likely_ requires instances from yaya-unsafe.
law_anaRefl ::
  (Eq t, Show t, Steppable (->) t f, Corecursive (->) t f, MonadTest m) =>
  t ->
  m ()
law_anaRefl = Law.anaRefl (===)
{- ORMOLU_DISABLE -}
DEPRECATE(law_anaRefl, "Use ‘anaRefl’ instead.", "yaya-hedgehog-0.4")
{- ORMOLU_ENABLE -}

anaRefl ::
  forall t f.
  (Eq t, Show t, Steppable (->) t f, Corecursive (->) t f) =>
  Gen t ->
  Property
anaRefl = property . (Law.anaRefl (===) <=< forAll)
{-# INLINEABLE anaRefl #-}

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
law_cataCompose = Law.cataCompose (===)
{- ORMOLU_DISABLE -}
DEPRECATE(law_cataCompose, "Use ‘cataCompose’ instead.", "yaya-hedgehog-0.4")
{- ORMOLU_ENABLE -}

cataCompose ::
  forall t a f u g proxy.
  ( Eq a,
    Show a,
    Show t,
    Recursive (->) t f,
    Steppable (->) u g,
    Recursive (->) u g
  ) =>
  proxy u ->
  Algebra (->) g a ->
  (forall x. f x -> g x) ->
  Gen t ->
  Property
cataCompose proxy φ ε = property . (Law.cataCompose (===) proxy φ ε <=< forAll)
{-# INLINEABLE cataCompose #-}

steppableOfHeight ::
  forall t n f.
  (Steppable (->) t f, Functor f, Recursive (->) n Maybe) =>
  -- | A generator for terminal cases (leaf nodes).
  Gen (f Void) ->
  -- | A generator for arbitrary cases. If the provided value generates terminal
  --   cases, then the resulting tree may have a height less than the `Size`,
  --   otherwise it will be a perfect tree with a height of exactly the provided
  --  `Size`.
  (Gen t -> Gen (f t)) ->
  n ->
  Gen t
steppableOfHeight leaf = cata . genAlgebra leaf

-- | Builds a generic tree generator of a certain height.
genAlgebra ::
  (Steppable (->) t f, Functor f) =>
  Gen (f Void) ->
  (Gen t -> Gen (f t)) ->
  Algebra (->) Maybe (Gen t)
genAlgebra leaf branch =
  maybe (fmap (embed . fmap absurd) leaf) (fmap embed . branch)
{-# INLINEABLE genAlgebra #-}

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
{-# INLINEABLE corecursiveIsUnsafe #-}

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
{-# INLINEABLE recursiveIsUnsafe #-}
