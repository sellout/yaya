{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   Common pattern functors (and instances for them).
--
--   This re-exports the functors from the strict library because it also adds
--   some orphan instances for them.
--
-- @since 0.1.0
module Yaya.Pattern
  ( module Data.Strict.Either,
    module Data.Strict.Maybe,
    module Data.Strict.Tuple,
    AndMaybe (Indeed, Only),
    XNor (Both, Neither),
    isNeither,
    andMaybe,
    xnor,
  )
where

import "base" Control.Applicative
  ( Applicative,
    liftA2,
    pure,
    (*>),
    (<*>),
    (<|>),
  )
import "base" Control.Category ((.))
import "base" Control.Monad (Monad, (>>=))
import "base" Data.Bifunctor (Bifunctor, bimap)
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
import "base" GHC.Read (expectP)
import "base" Text.Read (Read, parens, prec, readListPrec, readPrec, step)
import qualified "base" Text.Read.Lex as Lex
import "base" Text.Show (Show, showList, showParen, showString, showsPrec)
import "comonad" Control.Comonad (Comonad, duplicate, extract)
import "strict" Data.Strict.Either
  ( Either (Left, Right),
    either,
    fromLeft,
    fromRight,
    isLeft,
    isRight,
    lefts,
    partitionEithers,
    rights,
  )
import "strict" Data.Strict.Maybe
  ( Maybe (Just, Nothing),
    catMaybes,
    fromJust,
    fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    maybe,
    maybeToList,
  )
import "strict" Data.Strict.Tuple
  ( Pair ((:!:)),
    curry,
    fst,
    snd,
    swap,
    uncurry,
    unzip,
    zip,
    (:!:),
  )
import "base" Prelude ((+))

-- | Isomorphic to @'Maybe` (a, b)@, it’s also the pattern functor for lists.
--
-- @since 0.1.0
data XNor a b
  = -- | The empty case (@Nil@, for lists).
    --
    -- @since 0.1.0
    Neither
  | -- | Both an @a@ and a @b@ (@Cons@, for lists, where @b@ is the tail).
    --
    -- @since 0.1.0
    Both ~a b
  deriving stock
    ( -- | @since 0.5.1
      Eq,
      -- | @since 0.5.2.1
      Generic,
      -- | @since 0.5.1
      Ord,
      -- | @since 0.6.1
      Read,
      -- | @since 0.5.1
      Show,
      -- | @since 0.1.0
      Foldable,
      -- | @since 0.1.0
      Functor,
      -- | @since 0.5.2.1
      Generic1,
      -- | @since 0.1.0
      Traversable
    )

type role XNor representational representational

-- | Eliminator for `XNor`, akin to `Data.Either.either` or `Data.Maybe.maybe`.
--
-- @since 0.6.1
xnor :: c -> (a -> b -> c) -> XNor a b -> c
xnor neither both = \case
  Neither -> neither
  Both x y -> both x y

-- | Eliminator for `XNor`, akin to `Data.Either.isLeft` or `Data.Maybe.isNothing`.
--
-- @since 1.0.0
isNeither :: XNor a b -> Bool
isNeither = xnor True (\_ _ -> False)

-- | @since 0.5.1
instance (Eq a) => Eq1 (XNor a) where
  liftEq = liftEq2 (==)

-- | @since 0.5.1
instance Eq2 XNor where
  liftEq2 f g = Tuple.curry $ \case
    (Neither, Neither) -> True
    (Both x y, Both x' y') -> f x x' && g y y'
    (_, _) -> False

-- | @since 0.5.1
instance (Ord a) => Ord1 (XNor a) where
  liftCompare = liftCompare2 compare

-- | @since 0.5.1
instance Ord2 XNor where
  liftCompare2 f g = Tuple.curry $ \case
    (Neither, Neither) -> EQ
    (Neither, Both _ _) -> LT
    (Both _ _, Neither) -> GT
    (Both x y, Both x' y') -> f x x' <> g y y'

-- | @since 0.6.1
instance (Read a) => Read1 (XNor a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.6.1
instance Read2 XNor where
  liftReadPrec2 readPrecX _ readPrecY _ =
    let appPrec = 10
     in parens . prec appPrec $
          Neither <$ expectP (Lex.Ident "Neither")
            <|> expectP (Lex.Ident "Both")
              *> (Both <$> step readPrecX <*> step readPrecY)

-- | @since 0.5.1
instance (Show a) => Show1 (XNor a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 0.5.1
instance Show2 XNor where
  liftShowsPrec2 showsPrecX _ showsPrecY _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in xnor
          (showString "Neither")
          ( \x y ->
              showParen (nextPrec <= p) $
                showString "Both "
                  . showsPrecX nextPrec x
                  . showString " "
                  . showsPrecY nextPrec y
          )

-- | @since 0.1.0
instance Bifunctor XNor where
  bimap f g = xnor Neither (\a -> Both (f a) . g)

-- | Isomorphic to @(a, `Maybe` b)@, it’s also the pattern functor for non-empty
--   lists.
--
-- @since 0.1.0
data AndMaybe a b
  = -- | A lone @a@ (the last element, for non-empty lists).
    --
    -- @since 0.1.0
    Only ~a
  | -- | An @a@ followed by a @b@ (an element and the rest, for non-empty
    --   lists).
    --
    -- @since 0.1.0
    Indeed ~a b
  deriving stock
    ( -- | @since 0.5.1
      Eq,
      -- | @since 0.5.2.1
      Generic,
      -- | @since 0.6.1
      Read,
      -- | @since 0.5.1
      Show,
      -- | @since 0.1.0
      Foldable,
      -- | @since 0.1.0
      Functor,
      -- | @since 0.5.2.1
      Generic1,
      -- | @since 0.1.0
      Traversable
    )

type role AndMaybe representational representational

-- | Eliminator for `AndMaybe`, akin to `Data.Either.either` or
--  `Data.Maybe.maybe`.
--
-- @since 0.6.1
andMaybe :: (a -> c) -> (a -> b -> c) -> AndMaybe a b -> c
andMaybe only indeed = \case
  Only a -> only a
  Indeed a b -> indeed a b

-- | @since 0.5.1
instance (Eq a) => Eq1 (AndMaybe a) where
  liftEq = liftEq2 (==)

-- | @since 0.5.1
instance Eq2 AndMaybe where
  liftEq2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Indeed x y, Indeed x' y') -> f x x' && g y y'
    (_, _) -> False

-- | This definition is different from the one that is derivable. For example,
--   the derived instance would always have
--   @`compare` (`Only` x) (`Indeed` x' y) `==` `LT`@, but this instance will
--   return `GT` if @`compare` x x' `==` `GT`@.
--
-- @since 0.5.1
instance (Ord a, Ord b) => Ord (AndMaybe a b) where
  compare = liftCompare compare

-- | @since 0.5.1
instance (Ord a) => Ord1 (AndMaybe a) where
  liftCompare = liftCompare2 compare

-- | @since 0.5.1
instance Ord2 AndMaybe where
  liftCompare2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Only x, Indeed x' _) -> f x x' <> LT
    (Indeed x _, Only x') -> f x x' <> GT
    (Indeed x y, Indeed x' y') -> f x x' <> g y y'

-- | @since 0.6.1
instance (Read a) => Read1 (AndMaybe a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.6.1
instance Read2 AndMaybe where
  liftReadPrec2 readPrecX _ readPrecY _ =
    let appPrec = 10
     in parens . prec appPrec $
          expectP (Lex.Ident "Only") *> (Only <$> step readPrecX)
            <|> expectP (Lex.Ident "Indeed")
              *> (Indeed <$> step readPrecX <*> step readPrecY)

-- | @since 0.5.1
instance (Show a) => Show1 (AndMaybe a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 0.5.1
instance Show2 AndMaybe where
  liftShowsPrec2 showsPrecX _ showsPrecY _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in showParen (nextPrec <= p)
          . andMaybe
            (\x -> showString "Only " . showsPrecX nextPrec x)
            ( \x y ->
                showString "Indeed "
                  . showsPrecX nextPrec x
                  . showString " "
                  . showsPrecY nextPrec y
            )

-- | @since 0.1.0
instance Bifunctor AndMaybe where
  bimap f g = andMaybe (Only . f) (\a -> Indeed (f a) . g)

-- * orphan instances for types from the strict library

-- TODO: Explain why these instances are actually legit (fast & loose).

-- | @since 0.5.2.1
instance Applicative (Either a) where
  pure = Right
  liftA2 f = curry $ \case
    Right x :!: Right y -> Right $ f x y
    Right _ :!: Left a -> Left a
    Left a :!: _ -> Left a

-- | @since 0.5.2.1
instance Monad (Either a) where
  Left a >>= _ = Left a
  Right b >>= f = f b

-- | @since 0.5.2.1
instance Applicative Maybe where
  pure = Just
  liftA2 f = curry $ \case
    Just x :!: Just y -> Just $ f x y
    _ :!: _ -> Nothing

-- | @since 0.5.2.1
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a >>= f = f a

-- | @since 0.5.2.1
instance Comonad (Pair a) where
  extract = snd
  duplicate x@(a :!: _) = a :!: x
