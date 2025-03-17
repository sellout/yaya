{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains type classes that are defined as alternatives to the
--   ones in base. They are intended to remove dependence on native recursion
--   and partiality in cases where the classes from base impose that on the
--   implementor. There are instances of the classes from base in either
--   yaya-native or yaya-*:Yaya.*.Fold.Instances, depending on how egregious the
--   implementations must be.
module Yaya.Alternatives
  ( IsList (..),
    Monoid (..),
    Semigroup (..),
  )
where

import "base" Control.Category ((.))
import "base" Data.Function (flip, ($))
import "this" Yaya.Applied (append, coappend, foldr1, stimes')
import "this" Yaya.Fold
  ( Corecursive,
    Mu,
    Nu,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    cata2,
    embed,
    project,
  )
import "this" Yaya.Fold.Common (takeAvailable)
import "this" Yaya.Pattern (AndMaybe, Log2, Maybe, XNor (Neither), xnor)

class IsList l where
  type Item l
  fromList :: [Item l] -> l
  default fromList :: (Corecursive (->) l (XNor (Item l))) => [Item l] -> l
  fromList = ana project
  fromListN :: (Recursive (->) n Maybe) => n -> [Item l] -> l
  default fromListN ::
    (Steppable (->) l (XNor (Item l)), Recursive (->) n Maybe) =>
    n ->
    [Item l] ->
    l
  fromListN = cata2 (embed . takeAvailable)
  toList :: (Corecursive (->) t (XNor (Item l))) => l -> t
  default toList ::
    (Projectable (->) l (XNor (Item l)), Corecursive (->) t (XNor (Item l))) =>
    l ->
    t
  toList = ana project

-- FIXME: Move this to Native
-- -- | This instance is safe, since both structures are lazy.
-- --
-- --   $IsList
-- instance IsList (Cofix (XNor a)) where
--   type Item (Cofix (XNor a)) = a

-- | This instance is safe, since both structures are lazy.
--
--   $IsList
instance IsList (Nu (XNor a)) where
  type Item (Nu (XNor a)) = a

-- | `Data.Semigroup.Semigroup` requires both native and unsafe recursion in
--   places. This is an alternative specification that avoids those thing.s
class Semigroup a where
  (<>) :: a -> a -> a

  -- | `Data.Semigroup.sconcat` uses `NonEmpty`, which is both native and lazy,
  --   so not safe to fold. This abstracts over the non-empty type, allowing for
  --   recursion-free implementation.
  sconcat :: (Recursive (->) t (AndMaybe a)) => t -> a
  sconcat = foldr1 (<>)

  -- | `Data.Semigroup.stimes` uses `Int` for a positive number. This
  --   generalizes that and avoids the partiality of negative numbers and zero.
  stimes :: (Recursive (->) p Log2) => p -> a -> a
  stimes = flip $ stimes' (<>)

instance Semigroup (Mu (XNor a)) where
  (<>) = append

instance Semigroup (Nu (XNor a)) where
  (<>) = coappend

class (Semigroup a) => Monoid a where
  mempty :: a

  -- | `Data.Semigroup.mconcat` uses `[]`, which is both native and lazy, so not
  --   safe to fold. This abstracts over the non-empty type, allowing for
  --   recursion-free implementation.
  mconcat :: (Recursive (->) t (XNor a)) => t -> a
  mconcat = cata (xnor mempty (<>))

instance Monoid (Mu (XNor a)) where
  mempty = embed Neither

instance Monoid (Nu (XNor a)) where
  mempty = embed Neither
