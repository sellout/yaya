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
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (Read, expectP, parens, readPrec)
import "base" Text.ParserCombinators.ReadPrec (prec, step)
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show, showParen, showString, showsPrec)
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

data IntSetF r
  = NilF
  | TipF IntSet.Prefix IntSet.BitMap
  | BinF IntSet.Prefix IntSet.Mask r r
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

instance Projectable (->) IntSet.IntSet IntSetF where
  project IntSet.Nil = NilF
  project (IntSet.Tip prefix bm) = TipF prefix bm
  project (IntSet.Bin prefix mask l r) = BinF prefix mask l r

instance Recursive (->) IntSet.IntSet IntSetF where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) IntSet.IntSet IntSetF where
  embed NilF = IntSet.Nil
  embed (TipF prefix bm) = IntSet.Tip prefix bm
  embed (BinF prefix mask l r) = IntSet.Bin prefix mask l r

instance Eq1 IntSetF where
  liftEq f = Tuple.curry $ \case
    (NilF, NilF) -> True
    (TipF prefix bm, TipF prefix' bm') -> prefix == prefix' && bm == bm'
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && f l l' && f r r'
    (_, _) -> False

instance Ord1 IntSetF where
  liftCompare f = Tuple.curry $ \case
    (NilF, NilF) -> EQ
    (NilF, _) -> LT
    (TipF {}, NilF) -> GT
    (TipF prefix bm, TipF prefix' bm') ->
      compare prefix prefix' <> compare bm bm'
    (TipF {}, BinF {}) -> LT
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> f l l' <> f r r'
    (BinF {}, _) -> GT

-- | @since 0.1.2.0
instance Read1 IntSetF where
  liftReadPrec readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          NilF
            <$ expectP (Lex.Ident "NilF")
            <|> expectP (Lex.Ident "TipF")
              *> (TipF <$> step readPrec <*> step readPrec)
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF
                     <$> step readPrec
                     <*> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )

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
