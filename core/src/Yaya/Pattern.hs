{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Common pattern functors (and instances for them).
--
--   This re-exports the functors from the strict library because it also adds
--   some orphan instances for them.
module Yaya.Pattern
  ( module Data.Strict.Either,
    module Data.Strict.Maybe,
    module Data.Strict.Tuple,
    AndMaybe (Indeed, Only),
    Log2 (One, Double, DoublePlus),
    XNor (Both, Neither),
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
import "base" Data.Functor (Functor, ($>), (<$), (<$>))
import "base" Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2,
    Ord1,
    Ord2,
    Read1,
    Read2,
    Show1 (liftShowsPrec),
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
data XNor a b = Neither | Both ~a b
  deriving stock
    ( Eq,
      Generic,
      Ord,
      -- | @since 0.6.1.0
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

-- | Eliminator for `XNor`, akin to `Data.Either.either` or `Data.Maybe.maybe`.
--
--   @since 0.6.1.0
xnor :: c -> (a -> b -> c) -> XNor a b -> c
xnor neither both = \case
  Neither -> neither
  Both x y -> both x y

instance (Eq a) => Eq1 (XNor a) where
  liftEq = liftEq2 (==)

instance Eq2 XNor where
  liftEq2 f g = Tuple.curry $ \case
    (Neither, Neither) -> True
    (Both x y, Both x' y') -> f x x' && g y y'
    (_, _) -> False

instance (Ord a) => Ord1 (XNor a) where
  liftCompare = liftCompare2 compare

instance Ord2 XNor where
  liftCompare2 f g = Tuple.curry $ \case
    (Neither, Neither) -> EQ
    (Neither, Both _ _) -> LT
    (Both _ _, Neither) -> GT
    (Both x y, Both x' y') -> f x x' <> g y y'

-- | @since 0.6.1.0
instance (Read a) => Read1 (XNor a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.6.1.0
instance Read2 XNor where
  liftReadPrec2 readPrecX _ readPrecY _ =
    let appPrec = 10
     in parens . prec appPrec $
          Neither <$ expectP (Lex.Ident "Neither")
            <|> expectP (Lex.Ident "Both")
              *> (Both <$> step readPrecX <*> step readPrecY)

instance (Show a) => Show1 (XNor a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

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

instance Bifunctor XNor where
  bimap f g = xnor Neither (\a -> Both (f a) . g)

-- | Isomorphic to @(a, `Maybe` b)@, it’s also the pattern functor for non-empty
--   lists.
data AndMaybe a b = Only ~a | Indeed ~a b
  deriving stock
    ( Eq,
      Generic,
      -- | @since 0.6.1.0
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

-- | Eliminator for `AndMaybe`, akin to `Data.Either.either` or
--  `Data.Maybe.maybe`.
--
--   @since 0.6.1.0
andMaybe :: (a -> c) -> (a -> b -> c) -> AndMaybe a b -> c
andMaybe only indeed = \case
  Only a -> only a
  Indeed a b -> indeed a b

instance (Eq a) => Eq1 (AndMaybe a) where
  liftEq = liftEq2 (==)

instance Eq2 AndMaybe where
  liftEq2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Indeed x y, Indeed x' y') -> f x x' && g y y'
    (_, _) -> False

-- | This definition is different from the one that is derivable. For example,
--   the derived instance would always have
--   @`compare` (`Only` x) (`Indeed` x' y) `==` `LT`@, but this instance will
--   return `GT` if @`compare` x x' `==` `GT`@.
instance (Ord a, Ord b) => Ord (AndMaybe a b) where
  compare = liftCompare compare

instance (Ord a) => Ord1 (AndMaybe a) where
  liftCompare = liftCompare2 compare

instance Ord2 AndMaybe where
  liftCompare2 f g = Tuple.curry $ \case
    (Only x, Only x') -> f x x'
    (Only x, Indeed x' _) -> f x x' <> LT
    (Indeed x _, Only x') -> f x x' <> GT
    (Indeed x y, Indeed x' y') -> f x x' <> g y y'

-- | @since 0.6.1.0
instance (Read a) => Read1 (AndMaybe a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec

-- | @since 0.6.1.0
instance Read2 AndMaybe where
  liftReadPrec2 readPrecX _ readPrecY _ =
    let appPrec = 10
     in parens . prec appPrec $
          expectP (Lex.Ident "Only") *> (Only <$> step readPrecX)
            <|> expectP (Lex.Ident "Indeed")
              *> (Indeed <$> step readPrecX <*> step readPrecY)

instance (Show a) => Show1 (AndMaybe a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

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

instance Bifunctor AndMaybe where
  bimap f g = andMaybe (Only . f) (\a -> Indeed (f a) . g)

data Log2 a
  = -- | 1
    One
  | -- | 2a
    Double a
  | -- | 2a + 1
    DoublePlus a
  deriving stock
    ( Eq,
      Generic,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

instance Eq1 Log2 where
  liftEq f = Tuple.curry $ \case
    (One, One) -> True
    (Double x, Double x') -> f x x'
    (DoublePlus x, DoublePlus x') -> f x x'
    (_, _) -> False

-- | This definition is different from the one that is derivable. For example,
--   the derived instance would always have
--   @`compare` (`Double` x) (`DoublePlus` x') `==` `LT`@, but this instance
--   will return `GT` if @`compare` x x' `==` `GT`@.
instance (Ord a) => Ord (Log2 a) where
  compare = liftCompare compare

instance Ord1 Log2 where
  liftCompare f = Tuple.curry \case
    (One, One) -> EQ
    (One, _) -> LT
    (_, One) -> GT
    (Double x, Double x') -> f x x'
    (Double x, DoublePlus x') -> f x x' <> LT
    (DoublePlus x, Double x') -> f x x' <> GT
    (DoublePlus x, DoublePlus x') -> f x x'

-- | @since 0.6.1.0
instance Read1 Log2 where
  liftReadPrec readPrecX _ =
    let appPrec = 10
     in parens . prec appPrec $
          expectP (Lex.Ident "One") $> One
            <|> expectP (Lex.Ident "Double")
              *> (Double <$> step readPrecX)
            <|> expectP (Lex.Ident "DoublePlus")
              *> (DoublePlus <$> step readPrecX)

instance Show1 Log2 where
  liftShowsPrec showsPrecX _ p =
    let appPrec = 10
        nextPrec = appPrec + 1
     in showParen (nextPrec <= p)
          . \case
            One -> showString "One"
            Double x -> showString "Double " . showsPrecX nextPrec x
            DoublePlus x -> showString "DoublePlus " . showsPrecX nextPrec x

-- * orphan instances for types from the strict library

-- TODO: Explain why these instances are actually legit (fast & loose).

instance Applicative (Either a) where
  pure = Right
  liftA2 f = curry $ \case
    Right x :!: Right y -> Right $ f x y
    Right _ :!: Left a -> Left a
    Left a :!: _ -> Left a

instance Monad (Either a) where
  Left a >>= _ = Left a
  Right b >>= f = f b

instance Applicative Maybe where
  pure = Just
  liftA2 f = curry $ \case
    Just x :!: Just y -> Just $ f x y
    _ :!: _ -> Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just a >>= f = f a

instance Comonad (Pair a) where
  extract = snd
  duplicate x@(a :!: _) = a :!: x
