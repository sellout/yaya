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
module Yaya.Containers.Pattern.IntMap
  ( IntMap (Bin, Nil, Tip),
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
import qualified "containers" Data.IntMap.Internal as IntMap
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
import qualified "containers" Data.IntSet.Internal.IntTreeCommons as IntMap
  ( Prefix (Prefix),
  )
#endif

-- | The pattern functor of `IntMap.IntMap`, a big-endian Patricia tree. Each
--   constructor mirrors the one of the same name in "Data.IntMap.Internal",
--   with the recursive positions abstracted to @r@.
--
-- @since 1.0.0
data IntMap a r
  = -- | The empty map – `IntMap.Nil`.
    --
    -- @since 1.0.0
    Nil
  | -- | A single key and its value – `IntMap.Tip`.
    --
    -- @since 1.0.0
    Tip IntMap.Key a
#if MIN_VERSION_containers(0, 8, 0)
  | -- | A branch – `IntMap.Bin`: the `IntMap.Prefix` of the keys below it
    --   (which, as of @containers-0.8@, also encodes the bit at which they
    --   branch), then the left and right subtrees.
    --
    -- @since 1.0.0
    Bin IntMap.Prefix r r
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
  | -- | A branch – `IntMap.Bin`: the `IntMap.Prefix` shared by the keys below
    --   it, the `IntMap.Mask` selecting the bit at which they branch, then the
    --   left and right subtrees.
    --
    -- @since 1.0.0
    Bin IntMap.Prefix IntMap.Mask r r
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

type role IntMap representational representational

-- | @since 0.1.0
instance Projectable (->) (IntMap.IntMap a) (IntMap a) where
  project = \case
    IntMap.Nil -> Nil
    IntMap.Tip key a -> Tip key a
#if MIN_VERSION_containers(0, 8, 0)
    IntMap.Bin prefix l r -> Bin prefix l r
#else
    IntMap.Bin prefix mask l r -> Bin prefix mask l r
#endif

-- | @since 0.1.0
instance Recursive (->) (IntMap.IntMap a) (IntMap a) where
  cata = Unsafe.unsafeCata

-- | @since 0.1.0
instance Steppable (->) (IntMap.IntMap a) (IntMap a) where
  embed = \case
    Nil -> IntMap.Nil
    Tip key a -> IntMap.Tip key a
#if MIN_VERSION_containers(0, 8, 0)
    Bin prefix l r -> IntMap.Bin prefix l r
#else
    Bin prefix mask l r -> IntMap.Bin prefix mask l r
#endif

-- | @since 1.0.0
instance (Eq a) => Eq1 (IntMap a) where
  liftEq = liftEq2 (==)

-- | @since 1.0.0
instance Eq2 IntMap where
  liftEq2 f g = Tuple.curry $ \case
    (Nil, Nil) -> True
    (Nil, _) -> False
    (_, Nil) -> False
    (Tip key a, Tip key' a') -> key == key' && f a a'
    (Tip {}, _) -> False
    (_, Tip {}) -> False
#if MIN_VERSION_containers(0, 8, 0)
    (Bin prefix l r, Bin prefix' l' r') ->
      prefix == prefix' && g l l' && g r r'
#else
    (Bin prefix mask l r, Bin prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && g l l' && g r r'
#endif

-- | @since 1.0.0
instance (Ord a, Ord r) => Ord (IntMap a r) where
  compare = liftCompare compare

-- | @since 1.0.0
instance (Ord a) => Ord1 (IntMap a) where
  liftCompare = liftCompare2 compare

-- | @since 1.0.0
instance Ord2 IntMap where
  liftCompare2 f g = Tuple.curry $ \case
    (Nil, Nil) -> EQ
    (Nil, _) -> LT
    (Tip {}, Nil) -> GT
    (Tip key a, Tip key' a') -> compare key key' <> f a a'
    (Tip {}, Bin {}) -> LT
    (Bin {}, Nil) -> GT
    (Bin {}, Tip {}) -> GT
#if MIN_VERSION_containers(0, 8, 0)
    (Bin (IntMap.Prefix prefix) l r, Bin (IntMap.Prefix prefix') l' r') ->
      compare prefix prefix' <> g l l' <> g r r'
#else
    (Bin prefix mask l r, Bin prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> g l l' <> g r r'
#endif

-- | @since 1.0.0
instance (Read a, Read r) => Read (IntMap a r) where
  readPrec = liftReadPrec readPrec readListPrec

-- | @since 1.0.0
instance (Read a) => Read1 (IntMap a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 1.0.0
instance Read2 IntMap where
  liftReadPrec2 readPrecA _ readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          Nil
            <$ expectP (Lex.Ident "Nil")
            <|> expectP (Lex.Ident "TipF")
              *> (Tip <$> step readPrec <*> step readPrecA)
#if MIN_VERSION_containers(0, 8, 0)
            <|> expectP (Lex.Ident "Bin")
              *> ( Bin . IntMap.Prefix
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
instance (Show a, Show r) => Show (IntMap a r) where
  showsPrec = liftShowsPrec showsPrec showList

-- | @since 1.0.0
instance (Show a) => Show1 (IntMap a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 1.0.0
instance Show2 IntMap where
  liftShowsPrec2 showsPrecA _ showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          Nil -> showString "Nil"
          Tip key a ->
            showParen (nextPrec <= p) $
              showString "Tip "
                . showsPrec nextPrec key
                . showString " "
                . showsPrecA nextPrec a
#if MIN_VERSION_containers(0, 8, 0)
          Bin (IntMap.Prefix prefix) l r ->
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
