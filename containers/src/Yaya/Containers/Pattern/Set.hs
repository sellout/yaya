{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Containers.Pattern.Set
  ( Set (Bin, Tip),
  )
where

import "base" Control.Applicative ((*>), (<*>), (<|>))
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, (<$), (<$>))
import "base" Data.Functor.Classes
  ( Eq1,
    Eq2,
    Ord1,
    Ord2,
    Read1,
    Read2,
    Show1,
    Show2,
    liftCompare,
    liftCompare2,
    liftEq,
    liftEq2,
    liftReadPrec,
    liftReadPrec2,
    liftShowsPrec,
    liftShowsPrec2,
  )
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (Read, expectP, parens, readListPrec, readPrec)
import "base" Text.ParserCombinators.ReadPrec (prec, step)
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show, showList, showParen, showString, showsPrec)
import qualified "containers" Data.Set.Internal as Set
import "yaya" Yaya.Fold
  ( Projectable,
    Recursive,
    Steppable,
    cata,
    embed,
    project,
  )
import qualified "yaya-unsafe" Yaya.Unsafe.Fold as Unsafe
import "base" Prelude ((+))

-- | The pattern functor of `Set.Set`, a size-balanced binary search tree. Each
--   constructor mirrors the one of the same name in "Data.Set.Internal", with
--   the recursive positions abstracted to @r@.
--
-- @since 1.0.0
data Set a r
  = -- | The empty set – `Set.Tip`.
    --
    -- @since 1.0.0
    Tip
  | -- | A node – `Set.Bin`: the size of the subtree rooted here, its element,
    --   then the left and right subtrees.
    --
    -- @since 1.0.0
    Bin Set.Size a r r
  deriving stock
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Ord,
      -- | @since 1.0.0
      Generic,
      -- | @since 1.0.0
      Read,
      -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Foldable,
      -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Generic1,
      -- | @since 1.0.0
      Traversable
    )

type role Set representational representational

-- | @since 0.1.0
instance Projectable (->) (Set.Set a) (Set a) where
  project Set.Tip = Tip
  project (Set.Bin size a l r) = Bin size a l r

-- | @since 0.1.0
instance Recursive (->) (Set.Set a) (Set a) where
  cata = Unsafe.unsafeCata

-- | @since 0.1.0
instance Steppable (->) (Set.Set a) (Set a) where
  embed Tip = Set.Tip
  embed (Bin size a l r) = Set.Bin size a l r

-- | @since 1.0.0
instance (Eq a) => Eq1 (Set a) where
  liftEq = liftEq2 (==)

-- | @since 1.0.0
instance Eq2 Set where
  liftEq2 f g = Tuple.curry $ \case
    (Tip, Tip) -> True
    (Bin size a l r, Bin size' a' l' r') ->
      size == size' && f a a' && g l l' && g r r'
    (_, _) -> False

-- | @since 1.0.0
instance (Ord a) => Ord1 (Set a) where
  liftCompare = liftCompare2 compare

-- | @since 1.0.0
instance Ord2 Set where
  liftCompare2 f g = Tuple.curry $ \case
    (Tip, Tip) -> EQ
    (Tip, Bin {}) -> LT
    (Bin {}, Tip) -> GT
    (Bin size a l r, Bin size' a' l' r') ->
      compare size size' <> f a a' <> g l l' <> g r r'

-- | @since 1.0.0
instance (Read a) => Read1 (Set a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 1.0.0
instance Read2 Set where
  liftReadPrec2 readPrecA _ readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          Tip
            <$ expectP (Lex.Ident "Tip")
            <|> expectP (Lex.Ident "Bin")
              *> ( Bin
                     <$> step readPrec
                     <*> step readPrecA
                     <*> step readPrecR
                     <*> step readPrecR
                 )

-- | @since 1.0.0
instance (Show a) => Show1 (Set a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 1.0.0
instance Show2 Set where
  liftShowsPrec2 showsPrecA _ showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          Tip -> showString "Tip"
          Bin size a l r ->
            showParen (nextPrec <= p) $
              showString "Bin "
                . showsPrec nextPrec size
                . showString " "
                . showsPrecA nextPrec a
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
