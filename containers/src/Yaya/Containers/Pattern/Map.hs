{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Containers.Pattern.Map
  ( Map (Bin, Tip),
    eqMap,
    compareMap,
    readMapPrec,
    showsMapPrec,
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
import "base" Data.Int (Int)
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (expectP)
import "base" Text.Read
  ( Read,
    ReadPrec,
    parens,
    prec,
    readListPrec,
    readPrec,
    step,
  )
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show
  ( Show,
    ShowS,
    showList,
    showParen,
    showString,
    showsPrec,
  )
import qualified "containers" Data.Map.Internal as Map
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

-- | The pattern functor of `Map.Map`, a size-balanced binary search tree. Each
--   constructor mirrors the one of the same name in "Data.Map.Internal", with
--   the recursive positions abstracted to @r@.
--
-- @since 1.0.0
data Map k v r
  = -- | The empty map – `Map.Tip`.
    --
    -- @since 1.0.0
    Tip
  | -- | A node – `Map.Bin`: the size of the subtree rooted here, its key and
    --   (lazy) value, then the left and right subtrees.
    --
    -- @since 1.0.0
    Bin Map.Size k ~v r r
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

type role Map representational representational representational

-- | @since 0.1.0
instance Projectable (->) (Map.Map k v) (Map k v) where
  project Map.Tip = Tip
  project (Map.Bin size k v l r) = Bin size k v l r

-- | @since 0.1.0
instance Recursive (->) (Map.Map k v) (Map k v) where
  cata = Unsafe.unsafeCata

-- | @since 0.1.0
instance Steppable (->) (Map.Map k v) (Map k v) where
  embed Tip = Map.Tip
  embed (Bin size k v l r) = Map.Bin size k v l r

-- | Lifts equality tests on keys, values, and subtrees to a `Map` layer. Two
--   `Bin`s are equal when their sizes and all corresponding fields are.
--
-- @since 1.0.0
eqMap ::
  (k -> k' -> Bool) ->
  (v -> v' -> Bool) ->
  (r -> r' -> Bool) ->
  Map k v r ->
  Map k' v' r' ->
  Bool
eqMap eqK eqV eqR = Tuple.curry $ \case
  (Tip, Tip) -> True
  (Bin size k v l r, Bin size' k' v' l' r') ->
    size == size' && eqK k k' && eqV v v' && eqR l l' && eqR r r'
  (_, _) -> False

-- | Lifts comparisons of keys, values, and subtrees to a `Map` layer: `Tip`
--   precedes every `Bin`, and `Bin`s are compared lexicographically by size,
--   key, value, left subtree, then right subtree.
--
-- @since 1.0.0
compareMap ::
  (k -> k' -> Ordering) ->
  (v -> v' -> Ordering) ->
  (r -> r' -> Ordering) ->
  Map k v r ->
  Map k' v' r' ->
  Ordering
compareMap compareK compareV compareR = Tuple.curry $ \case
  (Tip, Tip) -> EQ
  (Tip, Bin {}) -> LT
  (Bin {}, Tip) -> GT
  (Bin size k v l r, Bin size' k' v' l' r') ->
    compare size size'
      <> compareK k k'
      <> compareV v v'
      <> compareR l l'
      <> compareR r r'

-- | Lifts parsers for keys, values, and subtrees to a `Map` layer, reading
--   the constructor syntax that `showsMapPrec` produces.
--
-- @since 1.0.0
readMapPrec :: ReadPrec k -> ReadPrec v -> ReadPrec r -> ReadPrec (Map k v r)
readMapPrec readPrecK readPrecV readPrecR =
  let appPrec = 10
   in parens . prec appPrec $
        Tip
          <$ expectP (Lex.Ident "Tip")
          <|> expectP (Lex.Ident "Bin")
            *> ( Bin
                   <$> step readPrec
                   <*> step readPrecK
                   <*> step readPrecV
                   <*> step readPrecR
                   <*> step readPrecR
               )

-- | Lifts printers for keys, values, and subtrees to a `Map` layer, rendering
--   it in constructor syntax at the given precedence.
--
-- @since 1.0.0
showsMapPrec ::
  (Int -> k -> ShowS) ->
  (Int -> v -> ShowS) ->
  (Int -> r -> ShowS) ->
  Int ->
  Map k v r ->
  ShowS
showsMapPrec showsPrecK showsPrecV showsPrecR p =
  let appPrec = 10
      nextPrec = appPrec + 1
   in \case
        Tip -> showString "Tip"
        Bin size k v l r ->
          showParen (nextPrec <= p) $
            showString "Bin "
              . showsPrec nextPrec size
              . showString " "
              . showsPrecK nextPrec k
              . showString " "
              . showsPrecV nextPrec v
              . showString " "
              . showsPrecR nextPrec l
              . showString " "
              . showsPrecR nextPrec r

-- | @since 1.0.0
instance (Eq k, Eq v) => Eq1 (Map k v) where
  liftEq = liftEq2 (==)

-- | @since 1.0.0
instance (Eq k) => Eq2 (Map k) where
  liftEq2 = eqMap (==)

-- | @since 1.0.0
instance (Ord k, Ord v) => Ord1 (Map k v) where
  liftCompare = liftCompare2 compare

-- | @since 1.0.0
instance (Ord k) => Ord2 (Map k) where
  liftCompare2 = compareMap compare

-- | @since 1.0.0
instance (Read k, Read v) => Read1 (Map k v) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 1.0.0
instance (Read k) => Read2 (Map k) where
  liftReadPrec2 readPrecV _ readPrecR _ =
    readMapPrec readPrec readPrecV readPrecR

-- | @since 1.0.0
instance (Show k, Show v) => Show1 (Map k v) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 1.0.0
instance (Show k) => Show2 (Map k) where
  liftShowsPrec2 showsPrecV _ showsPrecR _ =
    showsMapPrec showsPrec showsPrecV showsPrecR
