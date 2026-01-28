{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

data IntSet r
  = Nil
  | Tip Int IntSet.BitMap
#if MIN_VERSION_containers(0, 8, 0)
  | Bin IntSet.Prefix r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#else
  | Bin IntSet.Prefix IntSet.Mask r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#endif

instance Projectable (->) IntSet.IntSet IntSet where
  project = \case
    IntSet.Nil -> Nil
    IntSet.Tip prefix bm -> Tip prefix bm
#if MIN_VERSION_containers(0, 8, 0)
    IntSet.Bin prefix l r -> Bin prefix l r
#else
    IntSet.Bin prefix mask l r -> Bin prefix mask l r
#endif

instance Recursive (->) IntSet.IntSet IntSet where
  cata = Unsafe.unsafeCata

instance Steppable (->) IntSet.IntSet IntSet where
  embed = \case
    Nil -> IntSet.Nil
    Tip prefix bm -> IntSet.Tip prefix bm
#if MIN_VERSION_containers(0, 8, 0)
    Bin prefix l r -> IntSet.Bin prefix l r
#else
    Bin prefix mask l r -> IntSet.Bin prefix mask l r
#endif

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

instance (Ord r) => Ord (IntSet r) where
  compare = liftCompare compare

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

-- | @since 0.1.2.0
instance (Read r) => Read (IntSet r) where
  readPrec = liftReadPrec readPrec readListPrec

-- | @since 0.1.2.0
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

instance (Show r) => Show (IntSet r) where
  showsPrec = liftShowsPrec showsPrec showList

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
