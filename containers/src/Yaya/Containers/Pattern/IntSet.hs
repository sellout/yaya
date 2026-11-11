{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
-- With @containers-0.8@, "Data.IntSet.Internal.IntTreeCommons" is only
--   inferred `Safe`.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
#endif
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.1.0
module Yaya.Containers.Pattern.IntSet
  ( IntSet (Bin, Nil, Tip),
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
    Ord1,
    Read1,
    Show1,
    liftCompare,
    liftEq,
    liftReadPrec,
    liftShowsPrec,
  )
import "base" Data.Int (Int)
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (Read, expectP, parens, readListPrec, readPrec)
import "base" Text.ParserCombinators.ReadPrec (prec, step)
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show, showList, showParen, showString, showsPrec)
import qualified "containers" Data.IntSet.Internal as IntSet
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
#if MIN_VERSION_containers(0, 8, 0)
import qualified "containers" Data.IntSet.Internal.IntTreeCommons as IntSet
  ( Prefix (Prefix),
  )
#endif

-- | The pattern functor of `IntSet.IntSet`, a big-endian Patricia tree with
--   bitmap leaves. Each constructor mirrors the one of the same name in
--   "Data.IntSet.Internal", with the recursive positions abstracted to @r@.
--
-- @since 1.0.0
data IntSet r
  = -- | The empty set – `IntSet.Nil`.
    --
    -- @since 1.0.0
    Nil
  | -- | A leaf – `IntSet.Tip`: the high-order bits shared by a run of
    --   members, and a `IntSet.BitMap` whose set bits mark which values with
    --   those high-order bits are present.
    --
    -- @since 1.0.0
    Tip Int IntSet.BitMap
#if MIN_VERSION_containers(0, 8, 0)
  | -- | A branch – `IntSet.Bin`: the `IntSet.Prefix` of the members below it
    --   (which, as of @containers-0.8@, also encodes the bit at which they
    --   branch), then the left and right subtrees.
    --
    -- @since 1.0.0
    Bin IntSet.Prefix r r
  deriving stock
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Generic,
      -- | @since 1.0.0
      Foldable,
      -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Generic1,
      -- | @since 1.0.0
      Traversable
    )
#else
  | -- | A branch – `IntSet.Bin`: the `IntSet.Prefix` shared by the members
    --   below it, the `IntSet.Mask` selecting the bit at which they branch,
    --   then the left and right subtrees.
    --
    -- @since 1.0.0
    Bin IntSet.Prefix IntSet.Mask r r
  deriving stock
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Generic,
      -- | @since 1.0.0
      Foldable,
      -- | @since 1.0.0
      Functor,
      -- | @since 1.0.0
      Generic1,
      -- | @since 1.0.0
      Traversable
    )
#endif

type role IntSet representational

-- | @since 0.1.0
instance Projectable (->) IntSet.IntSet IntSet where
  project = \case
    IntSet.Nil -> Nil
    IntSet.Tip prefix bm -> Tip prefix bm
#if MIN_VERSION_containers(0, 8, 0)
    IntSet.Bin prefix l r -> Bin prefix l r
#else
    IntSet.Bin prefix mask l r -> Bin prefix mask l r
#endif

-- | @since 0.1.0
instance Recursive (->) IntSet.IntSet IntSet where
  cata = Unsafe.unsafeCata

-- | @since 0.1.0
instance Steppable (->) IntSet.IntSet IntSet where
  embed = \case
    Nil -> IntSet.Nil
    Tip prefix bm -> IntSet.Tip prefix bm
#if MIN_VERSION_containers(0, 8, 0)
    Bin prefix l r -> IntSet.Bin prefix l r
#else
    Bin prefix mask l r -> IntSet.Bin prefix mask l r
#endif

-- | @since 1.0.0
instance Eq1 IntSet where
  liftEq f = Tuple.curry $ \case
    (Nil, Nil) -> True
    (Nil, _) -> False
    (_, Nil) -> False
    (Tip prefix bm, Tip prefix' bm') -> prefix == prefix' && bm == bm'
    (Tip {}, _) -> False
    (_, Tip {}) -> False
#if MIN_VERSION_containers(0, 8, 0)
    (Bin prefix l r, Bin prefix' l' r') ->
      prefix == prefix' && f l l' && f r r'
#else
    (Bin prefix mask l r, Bin prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && f l l' && f r r'
#endif

-- | @since 1.0.0
instance (Ord r) => Ord (IntSet r) where
  compare = liftCompare compare

-- | @since 1.0.0
instance Ord1 IntSet where
  liftCompare f = Tuple.curry $ \case
    (Nil, Nil) -> EQ
    (Nil, _) -> LT
    (Tip {}, Nil) -> GT
    (Tip prefix bm, Tip prefix' bm') ->
      compare prefix prefix' <> compare bm bm'
    (Tip {}, Bin {}) -> LT
    (Bin {}, Nil) -> GT
    (Bin {}, Tip {}) -> GT
#if MIN_VERSION_containers(0, 8, 0)
    (Bin (IntSet.Prefix prefix) l r, Bin (IntSet.Prefix prefix') l' r') ->
      compare prefix prefix' <> f l l' <> f r r'
#else
    (Bin prefix mask l r, Bin prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> f l l' <> f r r'
#endif

-- | @since 1.0.0
instance (Read r) => Read (IntSet r) where
  readPrec = liftReadPrec readPrec readListPrec

-- | @since 1.0.0
instance Read1 IntSet where
  liftReadPrec readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          Nil
            <$ expectP (Lex.Ident "Nil")
            <|> expectP (Lex.Ident "Tip")
              *> (Tip <$> step readPrec <*> step readPrec)
#if MIN_VERSION_containers(0, 8, 0)
            <|> expectP (Lex.Ident "Bin")
              *> ( Bin . IntSet.Prefix
                     <$> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )
#else
            <|> expectP (Lex.Ident "Bin")
              *> ( Bin
                     <$> step readPrec
                     <*> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )
#endif

-- | @since 1.0.0
instance (Show r) => Show (IntSet r) where
  showsPrec = liftShowsPrec showsPrec showList

-- | @since 1.0.0
instance Show1 IntSet where
  liftShowsPrec showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          Nil -> showString "Nil"
          Tip prefix bm ->
            showParen (nextPrec <= p) $
              showString "Tip "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrec nextPrec bm
#if MIN_VERSION_containers(0, 8, 0)
          Bin (IntSet.Prefix prefix) l r ->
            showParen (nextPrec <= p) $
              showString "Bin "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
#else
          Bin prefix mask l r ->
            showParen (nextPrec <= p) $
              showString "Bin "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrec nextPrec mask
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
#endif
