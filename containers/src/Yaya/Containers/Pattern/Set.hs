{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Set
  ( SetF (BinF, TipF),
  )
where

import "base" Control.Applicative
  ( Alternative ((<|>)),
    Applicative ((<*>)),
    (*>),
  )
import "base" Control.Category (Category ((.)))
import "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap), (<$), (<$>))
import "base" Data.Ord (Ord (compare, (<=)), Ordering (EQ, GT, LT))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (Read (readListPrec, readPrec), expectP, parens)
import "base" Text.ParserCombinators.ReadPrec (prec, step)
import qualified "base" Text.Read.Lex as Lex
import qualified "containers" Data.Set.Internal as Set
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "base" Prelude (Num ((+)))
#if MIN_VERSION_base(4, 18, 0)
import "base" Data.Functor.Classes
  ( Eq1,
    Eq2 (liftEq2),
    Ord1,
    Ord2 (liftCompare2),
    Read1 (liftReadPrec),
    Read2 (liftReadPrec2),
    Show1,
    Show2 (liftShowsPrec2),
  )
import "base" Text.Show (Show (showsPrec), showParen, showString)
#else
import "base" Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2 (liftEq2),
    Ord1 (liftCompare),
    Ord2 (liftCompare2),
    Read1 (liftReadPrec),
    Read2 (liftReadPrec2),
    Show1 (liftShowsPrec),
    Show2 (liftShowsPrec2),
  )
import "base" Text.Show (Show (showList, showsPrec), showParen, showString)
#endif

data SetF a r = TipF | BinF Set.Size a r r
  deriving stock
    ( Eq,
      Ord,
      Generic,
      -- | @since 0.1.1.0
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

instance Projectable (->) (Set.Set a) (SetF a) where
  project Set.Tip = TipF
  project (Set.Bin size a l r) = BinF size a l r

instance Recursive (->) (Set.Set a) (SetF a) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (Set.Set a) (SetF a) where
  embed TipF = Set.Tip
  embed (BinF size a l r) = Set.Bin size a l r

#if MIN_VERSION_base(4, 18, 0)
instance (Eq a) => Eq1 (SetF a)
#else
instance (Eq a) => Eq1 (SetF a) where
  liftEq = liftEq2 (==)
#endif

instance Eq2 SetF where
  liftEq2 f g = Tuple.curry $ \case
    (TipF, TipF) -> True
    (BinF size a l r, BinF size' a' l' r') ->
      size == size' && f a a' && g l l' && g r r'
    (_, _) -> False

#if MIN_VERSION_base(4, 18, 0)
instance (Ord a) => Ord1 (SetF a)
#else
instance (Ord a) => Ord1 (SetF a) where
  liftCompare = liftCompare2 compare
#endif

instance Ord2 SetF where
  liftCompare2 f g = Tuple.curry $ \case
    (TipF, TipF) -> EQ
    (TipF, BinF {}) -> LT
    (BinF {}, TipF) -> GT
    (BinF size a l r, BinF size' a' l' r') ->
      compare size size' <> f a a' <> g l l' <> g r r'

-- | @since 0.1.1.0
instance (Read a) => Read1 (SetF a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.1.0
instance Read2 SetF where
  liftReadPrec2 readPrecA _ readPrecR _ =
    let appPrec = 10
     in parens . prec appPrec $
          TipF
            <$ expectP (Lex.Ident "TipF")
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF
                     <$> step readPrec
                     <*> step readPrecA
                     <*> step readPrecR
                     <*> step readPrecR
                 )

#if MIN_VERSION_base(4, 18, 0)
instance (Show a) => Show1 (SetF a)
#else
instance (Show a) => Show1 (SetF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
#endif

instance Show2 SetF where
  liftShowsPrec2 showsPrecA _ showsPrecR _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in \case
          TipF -> showString "TipF"
          BinF size a l r ->
            showParen (nextPrec <= p) $
              showString "BinF "
                . showsPrec nextPrec size
                . showString " "
                . showsPrecA nextPrec a
                . showString " "
                . showsPrecR nextPrec l
                . showString " "
                . showsPrecR nextPrec r
