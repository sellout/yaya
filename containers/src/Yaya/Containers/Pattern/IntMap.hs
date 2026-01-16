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
import "base" Data.Ord (Ord (compare, (<=)), Ordering (EQ, GT, LT))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (Read (readListPrec, readPrec), expectP, parens)
import "base" Text.ParserCombinators.ReadPrec (prec, step)
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show (showList, showsPrec), showParen, showString)
import qualified "containers" Data.IntMap.Internal as IntMap
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "base" Prelude (Num ((+)))
#if MIN_VERSION_containers(0, 8, 0)
import qualified "containers" Data.IntSet.Internal.IntTreeCommons as IntMap
  ( Prefix (Prefix),
  )

data IntMapF a r
  = NilF
  | TipF IntMap.Key a
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
data IntMapF a r
  = NilF
  | TipF IntMap.Key a
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

instance Projectable (->) (IntMap.IntMap a) (IntMapF a) where
  project IntMap.Nil = NilF
  project (IntMap.Tip key a) = TipF key a
#if MIN_VERSION_containers(0, 8, 0)
  project (IntMap.Bin prefix l r) = BinF prefix l r
#else
  project (IntMap.Bin prefix mask l r) = BinF prefix mask l r
#endif

instance Recursive (->) (IntMap.IntMap a) (IntMapF a) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (IntMap.IntMap a) (IntMapF a) where
  embed NilF = IntMap.Nil
  embed (TipF key a) = IntMap.Tip key a
#if MIN_VERSION_containers(0, 8, 0)
  embed (BinF prefix l r) = IntMap.Bin prefix l r
#else
  embed (BinF prefix mask l r) = IntMap.Bin prefix mask l r
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
