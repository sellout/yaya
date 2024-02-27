{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.IntMap
  ( IntMapF (..),
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2 (liftEq2),
    Ord1 (liftCompare),
    Ord2 (liftCompare2),
    Show1 (liftShowsPrec),
    Show2 (liftShowsPrec2),
  )
import "base" Data.Ord (Ord (compare, (<=)), Ordering (EQ, GT, LT))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" Text.Show (Show (showList, showsPrec), showParen, showString)
import qualified "containers" Data.IntMap.Internal as IntMap
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "base" Prelude (Num ((+)))

data IntMapF a r
  = NilF
  | TipF IntMap.Key a
  | BinF IntMap.Prefix IntMap.Mask r r
  deriving (Eq, Ord, Generic, Show, Foldable, Functor, Generic1, Traversable)

instance Projectable (->) (IntMap.IntMap a) (IntMapF a) where
  project IntMap.Nil = NilF
  project (IntMap.Tip key a) = TipF key a
  project (IntMap.Bin prefix mask l r) = BinF prefix mask l r

instance Recursive (->) (IntMap.IntMap a) (IntMapF a) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (IntMap.IntMap a) (IntMapF a) where
  embed NilF = IntMap.Nil
  embed (TipF key a) = IntMap.Tip key a
  embed (BinF prefix mask l r) = IntMap.Bin prefix mask l r

instance (Eq a) => Eq1 (IntMapF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftEq = liftEq2 (==)

instance Eq2 IntMapF where
  liftEq2 f g = Tuple.curry $ \case
    (NilF, NilF) -> True
    (TipF key a, TipF key' a') -> key == key' && f a a'
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && g l l' && g r r'
    (_, _) -> False

instance (Ord a) => Ord1 (IntMapF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftCompare = liftCompare2 compare

instance Ord2 IntMapF where
  liftCompare2 f g = Tuple.curry $ \case
    (NilF, NilF) -> EQ
    (NilF, _) -> LT
    (TipF {}, NilF) -> GT
    (TipF key a, TipF key' a') -> compare key key' <> f a a'
    (TipF {}, BinF {}) -> LT
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> g l l' <> g r r'
    (BinF {}, _) -> GT

instance (Show a) => Show1 (IntMapF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 IntMapF where
  liftShowsPrec2 showsPrecA _showListA showsPrecR _showListR prec =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          NilF -> showString "NilF"
          TipF key a ->
            showParen (nextPrec <= prec) $
              showString "BipF "
                . showsPrec nextPrec key
                . showString " "
                . showsPrecA nextPrec a
          BinF prefix mask l r ->
            showParen (nextPrec <= prec) $
              showString "BinF "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrec nextPrec mask
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
