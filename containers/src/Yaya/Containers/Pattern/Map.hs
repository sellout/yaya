{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Map
  ( MapF (BinF, TipF),
    -- | @since 0.1.2.0
    eqMapF,
    -- | @since 0.1.2.0
    compareMapF,
    -- | @since 0.1.2.0
    readMapFPrec,
    -- | @since 0.1.2.0
    showsMapFPrec,
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
import "yaya" Yaya.Strict (Strict)
import "base" Prelude ((+))

type instance Strict Map.Map = 'False

type instance Strict (Map.Map _k) = 'False

-- |
--
--  __FIXME__: This is strict only if `v` is not non-strict.
type instance Strict (Map.Map _k _v) = 'True

data MapF k v r = TipF | BinF Map.Size k ~v r r
  deriving stock
    ( Eq,
      Ord,
      Generic,
      -- | @since 0.1.2.0
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type instance Strict MapF = 'False

type instance Strict (MapF _k) = 'False

-- |
--
--  __FIXME__: This is strict only if `v` is not non-strict.
type instance Strict (MapF _k _v) = 'True

-- |
--
--  __FIXME__: This is strict only if `v` is not non-strict.
type instance Strict (MapF _k _v _r) = 'True

instance Projectable (->) (Map.Map k v) (MapF k v) where
  project Map.Tip = TipF
  project (Map.Bin size k v l r) = BinF size k v l r

instance Recursive (->) (Map.Map k v) (MapF k v) where
  cata = Unsafe.unsafeCata

instance Steppable (->) (Map.Map k v) (MapF k v) where
  embed TipF = Map.Tip
  embed (BinF size k v l r) = Map.Bin size k v l r

eqMapF ::
  (k -> k' -> Bool) ->
  (v -> v' -> Bool) ->
  (r -> r' -> Bool) ->
  MapF k v r ->
  MapF k' v' r' ->
  Bool
eqMapF eqK eqV eqR = Tuple.curry $ \case
  (TipF, TipF) -> True
  (BinF size k v l r, BinF size' k' v' l' r') ->
    size == size' && eqK k k' && eqV v v' && eqR l l' && eqR r r'
  (_, _) -> False

compareMapF ::
  (k -> k' -> Ordering) ->
  (v -> v' -> Ordering) ->
  (r -> r' -> Ordering) ->
  MapF k v r ->
  MapF k' v' r' ->
  Ordering
compareMapF compareK compareV compareR = Tuple.curry $ \case
  (TipF, TipF) -> EQ
  (TipF, BinF {}) -> LT
  (BinF {}, TipF) -> GT
  (BinF size k v l r, BinF size' k' v' l' r') ->
    compare size size'
      <> compareK k k'
      <> compareV v v'
      <> compareR l l'
      <> compareR r r'

readMapFPrec :: ReadPrec k -> ReadPrec v -> ReadPrec r -> ReadPrec (MapF k v r)
readMapFPrec readPrecK readPrecV readPrecR =
  let appPrec = 10
   in parens . prec appPrec $
        TipF
          <$ expectP (Lex.Ident "TipF")
          <|> expectP (Lex.Ident "BinF")
            *> ( BinF
                   <$> step readPrec
                   <*> step readPrecK
                   <*> step readPrecV
                   <*> step readPrecR
                   <*> step readPrecR
               )

showsMapFPrec ::
  (Int -> k -> ShowS) ->
  (Int -> v -> ShowS) ->
  (Int -> r -> ShowS) ->
  Int ->
  MapF k v r ->
  ShowS
showsMapFPrec showsPrecK showsPrecV showsPrecR p =
  let appPrec = 10
      nextPrec = appPrec + 1
   in \case
        TipF -> showString "TipF"
        BinF size k v l r ->
          showParen (nextPrec <= p) $
            showString "BinF "
              . showsPrec nextPrec size
              . showString " "
              . showsPrecK nextPrec k
              . showString " "
              . showsPrecV nextPrec v
              . showString " "
              . showsPrecR nextPrec l
              . showString " "
              . showsPrecR nextPrec r

instance (Eq k, Eq v) => Eq1 (MapF k v) where
  liftEq = liftEq2 (==)

instance (Eq k) => Eq2 (MapF k) where
  liftEq2 = eqMapF (==)

instance (Ord k, Ord v) => Ord1 (MapF k v) where
  liftCompare = liftCompare2 compare

instance (Ord k) => Ord2 (MapF k) where
  liftCompare2 = compareMapF compare

-- | @since 0.1.2.0
instance (Read k, Read v) => Read1 (MapF k v) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.2.0
instance (Read k) => Read2 (MapF k) where
  liftReadPrec2 readPrecV _ readPrecR _ =
    readMapFPrec readPrec readPrecV readPrecR

instance (Show k, Show v) => Show1 (MapF k v) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k) => Show2 (MapF k) where
  liftShowsPrec2 showsPrecV _ showsPrecR _ =
    showsMapFPrec showsPrec showsPrecV showsPrecR
