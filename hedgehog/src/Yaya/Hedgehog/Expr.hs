{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Hedgehog.Expr
  ( Expr (Add, Lit, Mult),
    expression,
    genCofixExpr,
    genExpr,
    genExprLit,
    genExprOp,
    genFixExpr,
    genMuExpr,
    genNuExpr,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Functor (Functor, (<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.Kind (Type)
import safe "base" Data.Traversable (Traversable)
import safe "base" Text.Show (Show)
import safe "deriving-compat" Data.Eq.Deriving (deriveEq1)
import safe "deriving-compat" Text.Show.Deriving (deriveShow1)
import safe "hedgehog" Hedgehog (Gen, Size)
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe qualified "hedgehog" Hedgehog.Range as Range
import safe "yaya" Yaya.Fold (Mu, Nu, Steppable)
import safe "yaya" Yaya.Fold.Native (Cofix, Fix)
import safe "this" Yaya.Hedgehog.Fold (embeddableOfHeight)

-- | A pattern functor for simple arithmetic expressions – integer literals
--   closed under addition and multiplication.
--
-- @since 0.1.0
data Expr (a :: Type)
  = -- | An integer literal.
    --
    -- @since 0.1.0
    Lit Int
  | -- | The sum of two subexpressions.
    --
    -- @since 0.1.0
    Add a a
  | -- | The product of two subexpressions.
    --
    -- @since 0.1.0
    Mult a a
  deriving stock
    ( -- | @since 0.1.0
      Eq,
      -- | @since 0.1.0
      Show,
      -- | @since 0.1.0
      Functor,
      -- | @since 0.1.0
      Foldable,
      -- | @since 0.1.0
      Traversable
    )

type role Expr representational

deriveEq1 ''Expr
deriveShow1 ''Expr

-- | Generates a `Lit` between @-1000@ and @1000@ (with the range scaled
--   linearly by the `Size`).
--
-- @since 0.1.0
genExprLit :: Gen (Expr a)
genExprLit = Lit <$> Gen.int (Range.linear (-1000) 1000)

-- | Generates an `Add` or a `Mult` (with equal probability), using the
--   provided generator for both operands.
--
-- @since 0.1.0
genExprOp :: Gen a -> Gen (Expr a)
genExprOp a = Gen.choice [Add <$> a <*> a, Mult <$> a <*> a]

-- | Generates any layer of `Expr`, choosing between `genExprLit` and
--   `genExprOp` with weights 3:2.
--
-- @since 0.1.0
genExpr :: Gen a -> Gen (Expr a)
genExpr a = Gen.frequency [(3, genExprLit), (2, genExprOp a)]

-- | Generates an expression of height at most the given `Size`, in any
--   `Steppable` representation of `Expr`. See `embeddableOfHeight`, including
--   its caveat about negative `Size`s.
--
-- @since 0.1.0
expression :: (Steppable (->) t Expr) => Size -> Gen t
expression = embeddableOfHeight genExprLit genExpr

-- | `expression`, specialized to `Mu`.
--
-- @since 0.1.0
genMuExpr :: Size -> Gen (Mu Expr)
genMuExpr = expression

-- | `expression`, specialized to `Nu`.
--
-- @since 0.1.0
genNuExpr :: Size -> Gen (Nu Expr)
genNuExpr = expression

-- | `expression`, specialized to `Fix`.
--
-- @since 0.1.0
genFixExpr :: Size -> Gen (Fix Expr)
genFixExpr = expression

-- | `expression`, specialized to `Cofix`.
--
-- @since 0.2.1
genCofixExpr :: Size -> Gen (Cofix Expr)
genCofixExpr = expression
