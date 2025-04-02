{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.IntSet
  ( IntSetF (BinF, NilF, TipF),
  )
where

import "base" Control.Applicative ((*>), (<*>), (<|>))
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, (<$), (<$>))
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
import "base" Prelude ((+))
#if MIN_VERSION_containers(0, 8, 0)
import qualified "containers" Data.IntSet.Internal.IntTreeCommons as IntSet
  ( Prefix (Prefix),
  )

data IntSetF r
  = NilF
  | TipF Int IntSet.BitMap
  | BinF IntSet.Prefix r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#else
data IntSetF r
  = NilF
  | TipF Int IntSet.BitMap
  | BinF IntSet.Prefix IntSet.Mask r r
  deriving stock
    ( Eq,
      Generic,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )
#endif

instance Projectable (->) IntSet.IntSet IntSetF where
  project IntSet.Nil = NilF
  project (IntSet.Tip prefix bm) = TipF prefix bm
#if MIN_VERSION_containers(0, 8, 0)
  project (IntSet.Bin prefix l r) = BinF prefix l r
#else
  project (IntSet.Bin prefix mask l r) = BinF prefix mask l r
#endif

instance Recursive (->) IntSet.IntSet IntSetF where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) IntSet.IntSet IntSetF where
  embed NilF = IntSet.Nil
  embed (TipF prefix bm) = IntSet.Tip prefix bm
#if MIN_VERSION_containers(0, 8, 0)
  embed (BinF prefix l r) = IntSet.Bin prefix l r
#else
  embed (BinF prefix mask l r) = IntSet.Bin prefix mask l r
#endif

instance Eq1 IntSetF where
  liftEq f = Tuple.curry $ \case
    (NilF, NilF) -> True
    (NilF, _) -> False
    (_, NilF) -> False
    (TipF prefix bm, TipF prefix' bm') -> prefix == prefix' && bm == bm'
    (TipF {}, _) -> False
    (_, TipF {}) -> False
#if MIN_VERSION_containers(0, 8, 0)
    (BinF prefix l r, BinF prefix' l' r') ->
      prefix == prefix' && f l l' && f r r'
#else
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && f l l' && f r r'
#endif

instance (Ord r) => Ord (IntSetF r) where
  compare = liftCompare compare

instance Ord1 IntSetF where
  liftCompare f = Tuple.curry $ \case
    (NilF, NilF) -> EQ
    (NilF, _) -> LT
    (TipF {}, NilF) -> GT
    (TipF prefix bm, TipF prefix' bm') ->
      compare prefix prefix' <> compare bm bm'
    (TipF {}, BinF {}) -> LT
    (BinF {}, NilF) -> GT
    (BinF {}, TipF {}) -> GT
#if MIN_VERSION_containers(0, 8, 0)
    (BinF (IntSet.Prefix prefix) l r, BinF (IntSet.Prefix prefix') l' r') ->
      compare prefix prefix' <> f l l' <> f r r'
#else
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> f l l' <> f r r'
#endif

-- | @since 0.1.2.0
instance (Read r) => Read (IntSetF r) where
  readPrec = liftReadPrec readPrec readListPrec

-- | @since 0.1.2.0
instance Read1 IntSetF where
  liftReadPrec readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          NilF
            <$ expectP (Lex.Ident "NilF")
            <|> expectP (Lex.Ident "TipF")
              *> (TipF <$> step readPrec <*> step readPrec)
#if MIN_VERSION_containers(0, 8, 0)
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF . IntSet.Prefix
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

instance (Show r) => Show (IntSetF r) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show1 IntSetF where
  liftShowsPrec showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          NilF -> showString "NilF"
          TipF prefix bm ->
            showParen (nextPrec <= p) $
              showString "TipF "
                . showsPrec nextPrec prefix
                . showString " "
                . showsPrec nextPrec bm
#if MIN_VERSION_containers(0, 8, 0)
          BinF (IntSet.Prefix prefix) l r ->
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
