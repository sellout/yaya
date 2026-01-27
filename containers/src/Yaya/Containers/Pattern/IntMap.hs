{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.IntMap
  ( IntMapF (BinF, NilF, TipF),
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
import "yaya" Yaya.Strict (Strict)
import qualified "yaya-unsafe" Yaya.Unsafe.Fold as Unsafe
import "base" Prelude ((+))
#if MIN_VERSION_containers(0, 8, 0)
import qualified "containers" Data.IntSet.Internal.IntTreeCommons as IntMap
  ( Prefix (Prefix),
  )
#endif

type instance Strict IntMap.IntMap = 'False

-- |
--
--  __FIXME__: This is strict only if `a` is not non-strict.
type instance Strict (IntMap.IntMap _a) = 'True

data IntMapF a r
  = NilF
  | TipF IntMap.Key ~a
#if MIN_VERSION_containers(0, 8, 0)
  | BinF IntMap.Prefix r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#else
  | BinF IntMap.Prefix IntMap.Mask r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#endif

type instance Strict IntMapF = 'False

-- |
--
--  __FIXME__: This is strict only if `a` is not non-strict.
type instance Strict (IntMapF _a) = 'True

-- |
--
--  __FIXME__: This is strict only if `a` is not non-strict.
type instance Strict (IntMapF _a _r) = 'True

instance Projectable (->) (IntMap.IntMap a) (IntMapF a) where
  project = \case
    IntMap.Nil -> NilF
    IntMap.Tip key a -> TipF key a
#if MIN_VERSION_containers(0, 8, 0)
    IntMap.Bin prefix l r -> BinF prefix l r
#else
    IntMap.Bin prefix mask l r -> BinF prefix mask l r
#endif

instance Recursive (->) (IntMap.IntMap a) (IntMapF a) where
  cata = Unsafe.unsafeCata

instance Steppable (->) (IntMap.IntMap a) (IntMapF a) where
  embed = \case
    NilF -> IntMap.Nil
    TipF key a -> IntMap.Tip key a
#if MIN_VERSION_containers(0, 8, 0)
    BinF prefix l r -> IntMap.Bin prefix l r
#else
    BinF prefix mask l r -> IntMap.Bin prefix mask l r
#endif

instance (Eq a) => Eq1 (IntMapF a) where
  liftEq = liftEq2 (==)

instance Eq2 IntMapF where
  liftEq2 f g = Tuple.curry $ \case
    (NilF, NilF) -> True
    (NilF, _) -> False
    (_, NilF) -> False
    (TipF key a, TipF key' a') -> key == key' && f a a'
    (TipF {}, _) -> False
    (_, TipF {}) -> False
#if MIN_VERSION_containers(0, 8, 0)
    (BinF prefix l r, BinF prefix' l' r') ->
      prefix == prefix' && g l l' && g r r'
#else
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && g l l' && g r r'
#endif

instance (Ord a, Ord r) => Ord (IntMapF a r) where
  compare = liftCompare compare

instance (Ord a) => Ord1 (IntMapF a) where
  liftCompare = liftCompare2 compare

instance Ord2 IntMapF where
  liftCompare2 f g = Tuple.curry $ \case
    (NilF, NilF) -> EQ
    (NilF, _) -> LT
    (TipF {}, NilF) -> GT
    (TipF key a, TipF key' a') -> compare key key' <> f a a'
    (TipF {}, BinF {}) -> LT
    (BinF {}, NilF) -> GT
    (BinF {}, TipF {}) -> GT
#if MIN_VERSION_containers(0, 8, 0)
    (BinF (IntMap.Prefix prefix) l r, BinF (IntMap.Prefix prefix') l' r') ->
      compare prefix prefix' <> g l l' <> g r r'
#else
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> g l l' <> g r r'
#endif

-- | @since 0.1.2.0
instance (Read a, Read r) => Read (IntMapF a r) where
  readPrec = liftReadPrec readPrec readListPrec

-- | @since 0.1.2.0
instance (Read a) => Read1 (IntMapF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.2.0
instance Read2 IntMapF where
  liftReadPrec2 readPrecA _ readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          NilF
            <$ expectP (Lex.Ident "NilF")
            <|> expectP (Lex.Ident "TipF")
              *> (TipF <$> step readPrec <*> step readPrecA)
#if MIN_VERSION_containers(0, 8, 0)
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF . IntMap.Prefix
                     <$> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )
#else
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF
                     <$> step readPrec
                     <*> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )
#endif

instance (Show a, Show r) => Show (IntMapF a r) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Show a) => Show1 (IntMapF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 IntMapF where
  liftShowsPrec2 showsPrecA _ showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          NilF -> showString "NilF"
          TipF key a ->
            showParen (nextPrec <= p) $
              showString "TipF "
                . showsPrec nextPrec key
                . showString " "
                . showsPrecA nextPrec a
#if MIN_VERSION_containers(0, 8, 0)
          BinF (IntMap.Prefix prefix) l r ->
            showParen (nextPrec <= p) $
              showString "BinF "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
#else
          BinF prefix mask l r ->
            showParen (nextPrec <= p) $
              showString "BinF "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrec nextPrec mask
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
#endif
