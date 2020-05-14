{-# options_ghc -Wno-orphans #-}

-- | Type class instances that use direct recursion in a potentially partial
--   way. This is separated from the rest of `Yaya.Unsafe.Fold` because you can
--   neither control nor qualify the import of instances. Therefore this module
--   is _extra_ dangerous, as having these instances available applies to the
--   entire module they’re imported into.
--
--   This contains instances that you might _expect_ to see, but which aren’t
--   actually total. For example, folding a lazy list `[a]` is _not_ guaranteed
--   to terminate.
module Yaya.Unsafe.Fold.Instances where

import           Control.Comonad.Cofree
import           Control.Comonad.Env
import           Control.Monad.Trans.Free
import           Data.Functor.Classes
import           Data.List.NonEmpty

import           Yaya.Fold
import           Yaya.Fold.Native
import           Yaya.Pattern
import qualified Yaya.Unsafe.Fold as Unsafe

instance Functor f => Recursive (->) (Fix f) f where
  cata = flip Unsafe.hylo project

instance (Functor f, Foldable f, Eq1 f) => Eq (Fix f) where
  (==) = recursiveEq

instance (Functor f, Show1 f) => Show (Fix f) where
  showsPrec = recursiveShowsPrec

instance Functor f => Corecursive (->) (Mu f) f where
  ana = Unsafe.hylo embed

instance Functor f => Recursive (->) (Nu f) f where
  cata = flip Unsafe.hylo project

instance (Functor f, Foldable f, Eq1 f) => Eq (Nu f) where
  (==) = recursiveEq

instance (Functor f, Show1 f) => Show (Nu f) where
  showsPrec = recursiveShowsPrec

instance Recursive (->) [a] (XNor a) where
  cata = flip Unsafe.hylo project

instance Recursive (->) (NonEmpty a) (AndMaybe a) where
  cata = flip Unsafe.hylo project

instance Functor f => Recursive (->) (Cofree f a) (EnvT a f) where
  cata = flip Unsafe.hylo project

instance Functor f => Recursive (->) (Free f a) (FreeF f a) where
  cata = flip Unsafe.hylo project

-- TODO: If we can generalize this to an arbitrary 'Recursive (->) t (FreeF h a)'
--       then it would no longer be unsafe.
seqFreeT
  :: (Functor f, Functor h)
  => DistributiveLaw (->) h f
  -> DistributiveLaw (->) (Free h) f
seqFreeT k =
  cata
  (\case
      Pure a -> free . Pure <$> a
      Free ft -> free . Free <$> k ft)
