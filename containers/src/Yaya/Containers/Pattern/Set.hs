{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

data Set a r = Tip | Bin Set.Size a r r
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

instance Projectable (->) (Set.Set a) (Set a) where
  project Set.Tip = Tip
  project (Set.Bin size a l r) = Bin size a l r

instance Recursive (->) (Set.Set a) (Set a) where
  cata = Unsafe.unsafeCata

instance Steppable (->) (Set.Set a) (Set a) where
  embed Tip = Set.Tip
  embed (Bin size a l r) = Set.Bin size a l r

instance (Eq a) => Eq1 (Set a) where
  liftEq = liftEq2 (==)

instance Eq2 Set where
  liftEq2 f g = Tuple.curry $ \case
    (Tip, Tip) -> True
    (Bin size a l r, Bin size' a' l' r') ->
      size == size' && f a a' && g l l' && g r r'
    (_, _) -> False

instance (Ord a) => Ord1 (Set a) where
  liftCompare = liftCompare2 compare

instance Ord2 Set where
  liftCompare2 f g = Tuple.curry $ \case
    (Tip, Tip) -> EQ
    (Tip, Bin {}) -> LT
    (Bin {}, Tip) -> GT
    (Bin size a l r, Bin size' a' l' r') ->
      compare size size' <> f a a' <> g l l' <> g r r'

-- | @since 0.1.2.0
instance (Read a) => Read1 (Set a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.2.0
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

instance (Show a) => Show1 (Set a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

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
