{-# LANGUAGE Safe #-}

module Yaya.Fold.Law
  ( anaRefl,
    cataCancel,
    cataCompose,
    cataFusion,
    cataRefl,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap, first)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "this" Yaya.Fold
  ( Algebra,
    Corecursive (ana),
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "this" Yaya.Fold.Common (diagonal)
import "this" Yaya.Pattern (uncurry)

cataCancel ::
  forall t a r f.
  ( Recursive (->) t f,
    Steppable (->) t f,
    Functor f
  ) =>
  (a -> a -> r) ->
  Algebra (->) f a ->
  f t ->
  r
cataCancel cmp φ =
  uncurry cmp . bimap (cata φ . embed) (φ . fmap (cata φ)) . diagonal
{-# INLINEABLE cataCancel #-}

cataRefl ::
  forall t r f.
  (Steppable (->) t f, Recursive (->) t f) =>
  (t -> t -> r) ->
  t ->
  r
cataRefl cmp = uncurry cmp . first (cata embed) . diagonal

-- |
--
--  __NB__: Since this requires both a `Corecursive` and `Eq` instance on the
--          same type, it /likely/ requires instances from yaya-unsafe.
anaRefl ::
  forall t r f.
  (Steppable (->) t f, Corecursive (->) t f) =>
  (t -> t -> r) ->
  t ->
  r
anaRefl cmp = uncurry cmp . first (ana project) . diagonal

cataCompose ::
  forall t a r f u g proxy.
  ( Recursive (->) t f,
    Steppable (->) u g,
    Recursive (->) u g
  ) =>
  (a -> a -> r) ->
  proxy u ->
  Algebra (->) g a ->
  (forall x. f x -> g x) ->
  t ->
  r
cataCompose cmp _ φ ε =
  uncurry cmp
    . bimap (cata φ . cata (embed . ε :: f u -> u)) (cata (φ . ε))
    . diagonal
{-# INLINEABLE cataCompose #-}

cataFusion ::
  forall t a r f.
  (Recursive (->) t f, Functor f) =>
  -- | comparison (often `==` or a check from some testing library, like
  --   Hedgehog).
  (a -> a -> r) ->
  -- | implication
  (r -> r -> r) ->
  (a -> a) ->
  Algebra (->) f a ->
  f a ->
  t ->
  r
cataFusion cmp imp f φ fa t =
  uncurry cmp (bimap (f . φ) (φ . fmap f) $ diagonal fa)
    `imp` uncurry cmp (bimap (f . cata φ) (cata φ) $ diagonal t)
