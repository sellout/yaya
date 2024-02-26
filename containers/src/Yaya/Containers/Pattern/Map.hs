{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Map
  ( MapF (..),
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
import qualified "containers" Data.Map.Internal as Map
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "base" Prelude (Num ((+)))

data MapF k v r = TipF | BinF Map.Size k ~v r r
  deriving (Eq, Ord, Generic, Show, Foldable, Functor, Generic1, Traversable)

instance Projectable (->) (Map.Map k v) (MapF k v) where
  project Map.Tip = TipF
  project (Map.Bin size k v l r) = BinF size k v l r

instance Recursive (->) (Map.Map k v) (MapF k v) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (Map.Map k v) (MapF k v) where
  embed TipF = Map.Tip
  embed (BinF size k v l r) = Map.Bin size k v l r

instance (Eq k, Eq v) => Eq1 (MapF k v) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftEq = liftEq2 (==)

instance (Eq k) => Eq2 (MapF k) where
  liftEq2 f g = Tuple.curry $ \case
    (TipF, TipF) -> True
    (BinF size k v l r, BinF size' k' v' l' r') ->
      size == size' && k == k' && f v v' && g l l' && g r r'
    (_, _) -> False

instance (Ord k, Ord v) => Ord1 (MapF k v) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftCompare = liftCompare2 compare

instance (Ord k) => Ord2 (MapF k) where
  liftCompare2 f g = Tuple.curry $ \case
    (TipF, TipF) -> EQ
    (TipF, BinF {}) -> LT
    (BinF {}, TipF) -> GT
    (BinF size k v l r, BinF size' k' v' l' r') ->
      compare size size' <> compare k k' <> f v v' <> g l l' <> g r r'

instance (Show k, Show v) => Show1 (MapF k v) where
  -- TODO: Remove this once base-4.18 is the oldest supported verson, as it’s
  --       the default impl.
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k) => Show2 (MapF k) where
  liftShowsPrec2 showsPrecV _showListV showsPrecR _showListR prec =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          TipF -> showString "TipF"
          BinF size k v l r ->
            showParen (nextPrec <= prec) $
              showString "BinF "
                . showsPrec nextPrec size
                . showString " "
                . showsPrec nextPrec k
                . showString " "
                . showsPrecV nextPrec v
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
