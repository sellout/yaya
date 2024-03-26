{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.IntMap
  ( IntMapF (BinF, NilF, TipF),
  )
where

import "base" Control.Applicative (Alternative ((<|>)), Applicative ((<*>)), (*>))
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
import qualified "containers" Data.IntMap.Internal as IntMap
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
    Ord2 (liftCompare2),
    Ord1,
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

data IntMapF a r
  = NilF
  | TipF IntMap.Key a
  | BinF IntMap.Prefix IntMap.Mask r r
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

instance Projectable (->) (IntMap.IntMap a) (IntMapF a) where
  project IntMap.Nil = NilF
  project (IntMap.Tip key a) = TipF key a
  project (IntMap.Bin prefix mask l r) = BinF prefix mask l r

instance Recursive (->) (IntMap.IntMap a) (IntMapF a) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (IntMap.IntMap a) (IntMapF a) where
  embed NilF = IntMap.Nil
  embed (TipF key a) = IntMap.Tip key a
  embed (BinF prefix mask l r) = IntMap.Bin prefix mask l r

#if MIN_VERSION_base(4, 18, 0)
instance (Eq a) => Eq1 (IntMapF a)
#else
instance (Eq a) => Eq1 (IntMapF a) where
  liftEq = liftEq2 (==)
#endif

instance Eq2 IntMapF where
  liftEq2 f g = Tuple.curry $ \case
    (NilF, NilF) -> True
    (TipF key a, TipF key' a') -> key == key' && f a a'
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      prefix == prefix' && mask == mask' && g l l' && g r r'
    (_, _) -> False

#if MIN_VERSION_base(4, 18, 0)
instance (Ord a) => Ord1 (IntMapF a)
#else
instance (Ord a) => Ord1 (IntMapF a) where
  liftCompare = liftCompare2 compare
#endif

instance Ord2 IntMapF where
  liftCompare2 f g = Tuple.curry $ \case
    (NilF, NilF) -> EQ
    (NilF, _) -> LT
    (TipF {}, NilF) -> GT
    (TipF key a, TipF key' a') -> compare key key' <> f a a'
    (TipF {}, BinF {}) -> LT
    (BinF prefix mask l r, BinF prefix' mask' l' r') ->
      compare prefix prefix' <> compare mask mask' <> g l l' <> g r r'
    (BinF {}, _) -> GT

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
            <|> expectP (Lex.Ident "BinF")
              *> ( BinF
                     <$> step readPrec
                     <*> step readPrec
                     <*> step readPrecR
                     <*> step readPrecR
                 )

#if MIN_VERSION_base(4, 18, 0)
instance (Show a) => Show1 (IntMapF a)
#else
instance (Show a) => Show1 (IntMapF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
#endif

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
