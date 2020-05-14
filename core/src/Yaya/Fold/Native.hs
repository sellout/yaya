{-# options_ghc -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Env
import Control.Monad.Trans.Free
import Data.List.NonEmpty
import Numeric.Natural

import Yaya.Fold
import Yaya.Pattern

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   lazy/corecursive.
newtype Fix f = Fix { unFix :: f (Fix f) }

instance Projectable (->) (Fix f) f where
  project = unFix

instance Steppable (->) (Fix f) f where
  embed = Fix

instance Functor f => Corecursive (->) (Fix f) f where
  ana φ = embed . fmap (ana φ) . φ

instance Recursive (->) Natural Maybe where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance Corecursive (->) [a] (XNor a) where
  ana ψ =
    (\case
        Neither  -> []
        Both h t -> h : ana ψ t)
    . ψ

instance Corecursive (->) (NonEmpty a) (AndMaybe a) where
  ana ψ =
    (\case
        Only h     -> h :| []
        Indeed h t -> h :| toList (ana ψ t))
    . ψ

instance Functor f => Corecursive (->) (Free f a) (FreeF f a) where
  ana ψ =
    free
    . (\case
          Pure a  -> Pure a
          Free fb -> Free . fmap (ana ψ) $ fb)
    . ψ

instance Functor f => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . runEnvT . ψ

distCofreeT
  :: (Functor f, Functor h)
  => DistributiveLaw (->) f h
  -> DistributiveLaw (->) f (Cofree h)
distCofreeT k = ana $ uncurry EnvT . (fmap extract &&& k . fmap unwrap)

