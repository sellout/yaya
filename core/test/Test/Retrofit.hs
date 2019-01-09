-- | The point of this module is that it should compile _without_ importing any
--   other Yaya modules.
module Test.Retrofit where

import qualified Yaya.Hedgehog.Expr as ExprF
import           Yaya.Retrofit

data DExpr
  = Lit Int
  | Add DExpr DExpr
  | Mult DExpr DExpr

instance Projectable DExpr ExprF.Expr where
  project = \case
    Lit i -> ExprF.Lit i
    Add a b -> ExprF.Add a b
    Mult a b -> ExprF.Mult a b

instance Steppable DExpr ExprF.Expr where
  embed = \case
    ExprF.Lit i -> Lit i
    ExprF.Add a b -> Add a b
    ExprF.Mult a b -> Mult a b

instance Corecursive DExpr ExprF.Expr where
  ana ψ = embed . fmap (ana ψ) . ψ

-- | This is unsafe, but we really just want to make sure all the methods are
--   available.
instance Recursive DExpr ExprF.Expr where
  cata φ = φ . fmap (cata φ) . project

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Eq DExpr where
  (==) = recursiveEq

-- | This can be derived in this case, but we want to ensure we could define it
--   if necessary.
instance Show DExpr where
  showsPrec = recursiveShowsPrec
