{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Map
  ( MapF (BinF, TipF),
    -- | @since 0.1.1.0
    eqMapF,
    -- | @since 0.1.1.0
    compareMapF,
    -- | @since 0.1.1.0
    readMapFPrec,
    -- | @since 0.1.1.0
    showsMapFPrec,
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
import "base" Data.Int (Int)
import "base" Data.Ord (Ord (compare, (<=)), Ordering (EQ, GT, LT))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (expectP)
import "base" Text.Read
  ( Read (readListPrec, readPrec),
    ReadPrec,
    parens,
    prec,
    step,
  )
import qualified "base" Text.Read.Lex as Lex
import qualified "containers" Data.Map.Internal as Map
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
import "base" Text.Show (Show (showsPrec), ShowS, showParen, showString)
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
import "base" Text.Show
  ( Show (showList, showsPrec),
    ShowS,
    showParen,
    showString,
  )
#endif

data MapF k v r = TipF | BinF Map.Size k ~v r r
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

instance Projectable (->) (Map.Map k v) (MapF k v) where
  project Map.Tip = TipF
  project (Map.Bin size k v l r) = BinF size k v l r

instance Recursive (->) (Map.Map k v) (MapF k v) where
  cata φ = φ . fmap (cata φ) . project

instance Steppable (->) (Map.Map k v) (MapF k v) where
  embed TipF = Map.Tip
  embed (BinF size k v l r) = Map.Bin size k v l r

eqMapF ::
  (k -> k' -> Bool) ->
  (v -> v' -> Bool) ->
  (r -> r' -> Bool) ->
  MapF k v r ->
  MapF k' v' r' ->
  Bool
eqMapF eqK eqV eqR = Tuple.curry $ \case
  (TipF, TipF) -> True
  (BinF size k v l r, BinF size' k' v' l' r') ->
    size == size' && eqK k k' && eqV v v' && eqR l l' && eqR r r'
  (_, _) -> False

compareMapF ::
  (k -> k' -> Ordering) ->
  (v -> v' -> Ordering) ->
  (r -> r' -> Ordering) ->
  MapF k v r ->
  MapF k' v' r' ->
  Ordering
compareMapF compareK compareV compareR = Tuple.curry $ \case
  (TipF, TipF) -> EQ
  (TipF, BinF {}) -> LT
  (BinF {}, TipF) -> GT
  (BinF size k v l r, BinF size' k' v' l' r') ->
    compare size size'
      <> compareK k k'
      <> compareV v v'
      <> compareR l l'
      <> compareR r r'

readMapFPrec :: ReadPrec k -> ReadPrec v -> ReadPrec r -> ReadPrec (MapF k v r)
readMapFPrec readPrecK readPrecV readPrecR =
  let appPrec = 10
   in parens . prec appPrec $
        TipF
          <$ expectP (Lex.Ident "TipF")
          <|> expectP (Lex.Ident "BinF")
            *> ( BinF
                   <$> step readPrec
                   <*> step readPrecK
                   <*> step readPrecV
                   <*> step readPrecR
                   <*> step readPrecR
               )

showsMapFPrec ::
  (Int -> k -> ShowS) ->
  (Int -> v -> ShowS) ->
  (Int -> r -> ShowS) ->
  Int ->
  MapF k v r ->
  ShowS
showsMapFPrec showsPrecK showsPrecV showsPrecR p =
  let appPrec = 10
      nextPrec = appPrec + 1
   in \case
        TipF -> showString "TipF"
        BinF size k v l r ->
          showParen (nextPrec <= p) $
            showString "BinF "
              . showsPrec nextPrec size
              . showString " "
              . showsPrecK nextPrec k
              . showString " "
              . showsPrecV nextPrec v
              . showString " "
              . showsPrecR nextPrec l
              . showString " "
              . showsPrecR nextPrec r

#if MIN_VERSION_base(4, 18, 0)
instance (Eq k, Eq v) => Eq1 (MapF k v)
#else
instance (Eq k, Eq v) => Eq1 (MapF k v) where
  liftEq = liftEq2 (==)
#endif

instance (Eq k) => Eq2 (MapF k) where
  liftEq2 = eqMapF (==)

#if MIN_VERSION_base(4, 18, 0)
instance (Ord k, Ord v) => Ord1 (MapF k v)
#else
instance (Ord k, Ord v) => Ord1 (MapF k v) where
  liftCompare = liftCompare2 compare
#endif

instance (Ord k) => Ord2 (MapF k) where
  liftCompare2 = compareMapF compare

-- | @since 0.1.1.0
instance (Read k, Read v) => Read1 (MapF k v) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.1.0
instance (Read k) => Read2 (MapF k) where
  liftReadPrec2 readPrecV _ readPrecR _ =
    readMapFPrec readPrec readPrecV readPrecR

#if MIN_VERSION_base(4, 18, 0)
instance (Show k, Show v) => Show1 (MapF k v)
#else
instance (Show k, Show v) => Show1 (MapF k v) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
#endif

instance (Show k) => Show2 (MapF k) where
  liftShowsPrec2 showsPrecV _ showsPrecR _ =
    showsMapFPrec showsPrec showsPrecV showsPrecR
