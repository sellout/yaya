{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Set
  ( SetF (..),
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
import qualified "containers" Data.Set.Internal as Set
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "base" Prelude (Num ((+)))

data SetF a r = TipF | BinF Set.Size a r r
  deriving (Eq, Ord, Generic, Show, Foldable, Functor, Generic1, Traversable)

instance Projectable (->) (Set.Set a) (SetF a) where
  project Set.Tip = TipF
  project (Set.Bin size a l r) = BinF size a l r

instance Recursive (->) (Set.Set a) (SetF a) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (Set.Set a) (SetF a) where
  embed TipF = Set.Tip
  embed (BinF size a l r) = Set.Bin size a l r

instance (Eq a) => Eq1 (SetF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftEq = liftEq2 (==)

instance Eq2 SetF where
  liftEq2 f g = Tuple.curry $ \case
    (TipF, TipF) -> True
    (BinF size a l r, BinF size' a' l' r') ->
      size == size' && f a a' && g l l' && g r r'
    (_, _) -> False

instance (Ord a) => Ord1 (SetF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftCompare = liftCompare2 compare

instance Ord2 SetF where
  liftCompare2 f g = Tuple.curry $ \case
    (TipF, TipF) -> EQ
    (TipF, BinF {}) -> LT
    (BinF {}, TipF) -> GT
    (BinF size a l r, BinF size' a' l' r') ->
      compare size size' <> f a a' <> g l l' <> g r r'

instance (Show a) => Show1 (SetF a) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 SetF where
  liftShowsPrec2 showsPrecA _showListA showsPrecR _showListR prec =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          TipF -> showString "TipF"
          BinF size a l r ->
            showParen (nextPrec <= prec) $
              showString "BinF "
                . showsPrec nextPrec size
                . showString " "
                . showsPrecA nextPrec a
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
