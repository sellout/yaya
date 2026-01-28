{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaya.Containers.Pattern.Map
  ( Map (Bin, Tip),
    -- | @since 0.1.2.0
    eqMap,
    -- | @since 0.1.2.0
    compareMap,
    -- | @since 0.1.2.0
    readMapPrec,
    -- | @since 0.1.2.0
    showsMapPrec,
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
import "base" Data.Int (Int)
import "base" Data.Ord (Ord, Ordering (EQ, GT, LT), compare, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import qualified "base" Data.Tuple as Tuple
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Read (expectP)
import "base" Text.Read
  ( Read,
    ReadPrec,
    parens,
    prec,
    readListPrec,
    readPrec,
    step,
  )
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show
  ( Show,
    ShowS,
    showList,
    showParen,
    showString,
    showsPrec,
  )
import qualified "containers" Data.Map.Internal as Map
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

data Map k v r = Tip | Bin Map.Size k ~v r r
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

instance Projectable (->) (Map.Map k v) (Map k v) where
  project Map.Tip = Tip
  project (Map.Bin size k v l r) = Bin size k v l r

instance Recursive (->) (Map.Map k v) (Map k v) where
  cata = Unsafe.unsafeCata

instance Steppable (->) (Map.Map k v) (Map k v) where
  embed Tip = Map.Tip
  embed (Bin size k v l r) = Map.Bin size k v l r

eqMap ::
  (k -> k' -> Bool) ->
  (v -> v' -> Bool) ->
  (r -> r' -> Bool) ->
  Map k v r ->
  Map k' v' r' ->
  Bool
eqMap eqK eqV eqR = Tuple.curry $ \case
  (Tip, Tip) -> True
  (Bin size k v l r, Bin size' k' v' l' r') ->
    size == size' && eqK k k' && eqV v v' && eqR l l' && eqR r r'
  (_, _) -> False

compareMap ::
  (k -> k' -> Ordering) ->
  (v -> v' -> Ordering) ->
  (r -> r' -> Ordering) ->
  Map k v r ->
  Map k' v' r' ->
  Ordering
compareMap compareK compareV compareR = Tuple.curry $ \case
  (Tip, Tip) -> EQ
  (Tip, Bin {}) -> LT
  (Bin {}, Tip) -> GT
  (Bin size k v l r, Bin size' k' v' l' r') ->
    compare size size'
      <> compareK k k'
      <> compareV v v'
      <> compareR l l'
      <> compareR r r'

readMapPrec :: ReadPrec k -> ReadPrec v -> ReadPrec r -> ReadPrec (Map k v r)
readMapPrec readPrecK readPrecV readPrecR =
  let appPrec = 10
   in parens . prec appPrec $
        Tip
          <$ expectP (Lex.Ident "Tip")
          <|> expectP (Lex.Ident "Bin")
            *> ( Bin
                   <$> step readPrec
                   <*> step readPrecK
                   <*> step readPrecV
                   <*> step readPrecR
                   <*> step readPrecR
               )

showsMapPrec ::
  (Int -> k -> ShowS) ->
  (Int -> v -> ShowS) ->
  (Int -> r -> ShowS) ->
  Int ->
  Map k v r ->
  ShowS
showsMapPrec showsPrecK showsPrecV showsPrecR p =
  let appPrec = 10
      nextPrec = appPrec + 1
   in \case
        Tip -> showString "Tip"
        Bin size k v l r ->
          showParen (nextPrec <= p) $
            showString "Bin "
              . showsPrec nextPrec size
              . showString " "
              . showsPrecK nextPrec k
              . showString " "
              . showsPrecV nextPrec v
              . showString " "
              . showsPrecR nextPrec l
              . showString " "
              . showsPrecR nextPrec r

instance (Eq k, Eq v) => Eq1 (Map k v) where
  liftEq = liftEq2 (==)

instance (Eq k) => Eq2 (Map k) where
  liftEq2 = eqMap (==)

instance (Ord k, Ord v) => Ord1 (Map k v) where
  liftCompare = liftCompare2 compare

instance (Ord k) => Ord2 (Map k) where
  liftCompare2 = compareMap compare

-- | @since 0.1.2.0
instance (Read k, Read v) => Read1 (Map k v) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.1.2.0
instance (Read k) => Read2 (Map k) where
  liftReadPrec2 readPrecV _ readPrecR _ =
    readMapPrec readPrec readPrecV readPrecR

instance (Show k, Show v) => Show1 (Map k v) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k) => Show2 (Map k) where
  liftShowsPrec2 showsPrecV _ showsPrecR _ =
    showsMapPrec showsPrec showsPrecV showsPrecR
