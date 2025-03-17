{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wwarn=deprecations #-}

#include "../../deprecation.h"

module Yaya.Hedgehog.Expr
  ( Expr (Add, Lit, Mult),
    anaRefl,
    cataCancel,
    cataCompose,
    cataRefl,
    exprInRange,
    exprOfHeight,
    genExpr,
    genExprLit,
    genExprOp,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, (<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.Traversable (Traversable)
import safe "base" Text.Show (Show)
import safe "deriving-compat" Data.Eq.Deriving (deriveEq1)
import safe "deriving-compat" Text.Show.Deriving (deriveShow1)
import safe "hedgehog" Hedgehog (Gen, Property, Range)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe qualified "hedgehog" Hedgehog.Range as Range
import safe "yaya" Yaya.Fold (Algebra, Corecursive, Recursive, Steppable)
import safe "yaya" Yaya.Pattern (Maybe)
import safe qualified "this" Yaya.Hedgehog.Fold as Fold
import safe "base" Prelude (Integral)

data Expr a
  = Lit Int
  | Add a a
  | Mult a a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Expr
deriveShow1 ''Expr

-- | Generates leaf `Expr` nodes.
genExprLit :: Gen (Expr a)
genExprLit = Lit <$> Gen.int (Range.linear (-1000) 1000)

-- genExprLit = Lit <$> Gen.integralBounded

-- | Generates non-leaf `Expr` nodes.
genExprOp :: Gen a -> Gen (Expr a)
genExprOp a = Gen.choice [Add <$> a <*> a, Mult <$> a <*> a]

-- | Generates any `Expr` node, with a flat distribution.
genExpr :: Gen a -> Gen (Expr a)
genExpr a = Gen.frequency [(1, genExprLit), (2, genExprOp a)]

-- | Generates a perfect `Expr` tree of the specified height.
--
--  __NB__: @`exprOfHeight` `<=<` `Gen.integral_`@ can be used to create `Expr`s
--          with a `Range` of heights.
exprOfHeight :: forall t n. (Recursive (->) n Maybe, Steppable (->) t Expr) => n -> Gen t
exprOfHeight = Fold.steppableOfHeight genExprLit genExprOp
{-# INLINEABLE exprOfHeight #-}

-- | This creates a generator for `Expr`s with heights in the given range.
--
--   This exists to make it slightly less likely that gigantic data structures
--   are created. I.e., an `Expr` of height /n/ has 2^n nodes, so this ensures
--   we use an exponential distribution, so only a few structures at the high
--   end of the range are created.
exprInRange ::
  forall t n.
  (Integral n, Recursive (->) n Maybe, Steppable (->) t Expr) =>
  (Range n -> Gen n) ->
  n ->
  n ->
  Gen t
exprInRange genN minHeight maxHeight =
  exprOfHeight <=< genN $ Range.exponential minHeight maxHeight

anaRefl ::
  forall t n proxy.
  ( Eq t,
    Show t,
    Integral n,
    Recursive (->) n Maybe,
    Steppable (->) t Expr,
    Corecursive (->) t Expr
  ) =>
  proxy t ->
  (Range n -> Gen n) ->
  n ->
  n ->
  Property
anaRefl _ genN minHeight maxHeight =
  Fold.anaRefl @t $ exprInRange genN minHeight maxHeight

cataCancel ::
  forall t n a proxy.
  ( Eq a,
    Show a,
    Show (Expr t),
    Integral n,
    Recursive (->) n Maybe,
    Recursive (->) t Expr,
    Steppable (->) t Expr
  ) =>
  proxy t ->
  Algebra (->) Expr a ->
  (Range n -> Gen n) ->
  n ->
  n ->
  Property
cataCancel _ φ genN minHeight maxHeight =
  Fold.cataCancel @t φ . genExpr $ exprInRange genN minHeight maxHeight
{-# INLINEABLE cataCancel #-}

cataCompose ::
  forall t n a u g proxy proxy'.
  ( Eq a,
    Show a,
    Show t,
    Integral n,
    Recursive (->) n Maybe,
    Steppable (->) t Expr,
    Recursive (->) t Expr,
    Steppable (->) u g,
    Recursive (->) u g
  ) =>
  proxy t ->
  proxy' u ->
  Algebra (->) g a ->
  (forall x. Expr x -> g x) ->
  (Range n -> Gen n) ->
  n ->
  n ->
  Property
cataCompose _ proxy φ ε genN minHeight maxHeight =
  Fold.cataCompose @t proxy φ ε $ exprInRange genN minHeight maxHeight

cataRefl ::
  forall t n proxy.
  ( Eq t,
    Show t,
    Integral n,
    Recursive (->) n Maybe,
    Steppable (->) t Expr,
    Recursive (->) t Expr
  ) =>
  proxy t ->
  (Range n -> Gen n) ->
  n ->
  n ->
  Property
cataRefl _ genN minHeight maxHeight =
  Fold.cataRefl @t $ exprInRange genN minHeight maxHeight
