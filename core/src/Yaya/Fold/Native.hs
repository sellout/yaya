-- NB: We disable @StrictData@ here in order for `Cofix` to be lazy. I don’t
--     think there is any way to explicitly add @~@ patterns that has the
--     correct semantics.
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Uses of recursion schemes that use Haskell’s built-in recursion in a total
--   manner.
module Yaya.Fold.Native where

import Control.Arrow
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Env
import Control.Monad.Trans.Free
import Data.Functor.Classes
import Data.List.NonEmpty
import Numeric.Natural
import Yaya.Fold
import Yaya.Pattern

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   strict/recursive.
newtype Fix f = Fix {unFix :: f (Fix f)}

instance Projectable (->) (Fix f) f where
  project = unFix

instance Steppable (->) (Fix f) f where
  embed = Fix

instance Functor f => Recursive (->) (Fix f) f where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance (Functor f, Foldable f, Eq1 f) => Eq (Fix f) where
  (==) = recursiveEq

instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

-- | A fixed-point constructor that uses Haskell's built-in recursion. This is
--   lazy/corecursive.
data Cofix f = Cofix {unCofix :: f (Cofix f)}
{-# ANN Cofix "HLint: ignore Use newtype instead of data" #-}

instance Projectable (->) (Cofix f) f where
  project = unCofix

instance Steppable (->) (Cofix f) f where
  embed = Cofix

instance Functor f => Corecursive (->) (Cofix f) f where
  ana φ = embed . fmap (ana φ) . φ

instance Recursive (->) Natural Maybe where
  cata ɸ = ɸ . fmap (cata ɸ) . project

instance Corecursive (->) [a] (XNor a) where
  ana ψ =
    ( \case
        Neither -> []
        Both h t -> h : ana ψ t
    )
      . ψ

instance Corecursive (->) (NonEmpty a) (AndMaybe a) where
  ana ψ =
    ( \case
        Only h -> h :| []
        Indeed h t -> h :| toList (ana ψ t)
    )
      . ψ

instance Functor f => Corecursive (->) (Free f a) (FreeF f a) where
  ana ψ =
    free
      . ( \case
            Pure a -> Pure a
            Free fb -> Free . fmap (ana ψ) $ fb
        )
      . ψ

instance Functor f => Corecursive (->) (Cofree f a) (EnvT a f) where
  ana ψ = uncurry (:<) . fmap (fmap (ana ψ)) . runEnvT . ψ

distCofreeT ::
  (Functor f, Functor h) =>
  DistributiveLaw (->) f h ->
  DistributiveLaw (->) f (Cofree h)
distCofreeT k = ana $ uncurry EnvT . (fmap extract &&& k . fmap unwrap)
