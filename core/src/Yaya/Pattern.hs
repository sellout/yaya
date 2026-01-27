{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
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
    EnvT (EnvT),
    FreeF (Pure, Free),
    XNor (Both, Neither),
    andMaybe,
    ask,
    lowerEnvT,
    runEnvT,
    unzip,
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
import "comonad" Control.Comonad (Comonad, duplicate, extend, extract)
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
    zip,
    (:!:),
  )
import "this" Yaya.Functor (HFunctor, hmap)
import "this" Yaya.Strict (Strict)
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

type instance Strict XNor = 'False

type instance Strict (XNor _a) = 'True

type instance Strict (XNor _a _b) = 'True

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

type instance Strict AndMaybe = 'False

type instance Strict (AndMaybe _a) = 'True

type instance Strict (AndMaybe _a _b) = 'True

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

-- | A strict environment transformer.
data EnvT e w a = EnvT {ask :: e, lowerEnvT :: w a}
  deriving stock
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type instance Strict EnvT = 'True

type instance Strict (EnvT _e) = 'True

type instance Strict (EnvT _e _w) = 'True

type instance Strict (EnvT _e _w _a) = 'True

instance (Eq e, Eq1 f) => Eq1 (EnvT e f) where
  liftEq eqA (EnvT e fa) (EnvT e' fa') = e == e' && liftEq eqA fa fa'

instance (Ord e, Ord1 f) => Ord1 (EnvT e f) where
  liftCompare compareA (EnvT e fa) (EnvT e' fa') =
    compare e e' <> liftCompare compareA fa fa'

instance (Read e, Read1 f) => Read1 (EnvT e f) where
  liftReadPrec readPrecA readListA =
    let appPrec = 10
     in parens . prec appPrec $
          expectP (Lex.Ident "EnvT")
            *> ( EnvT
                   <$> step readPrec
                   <*> step (liftReadPrec readPrecA readListA)
               )

instance (Show e, Show1 f) => Show1 (EnvT e f) where
  liftShowsPrec showsPrecA showListA p (EnvT e fa) =
    let appPrec = 10
        nextPrec = appPrec + 1
     in showParen (nextPrec <= p) $
          showString "EnvT "
            . showsPrec nextPrec e
            . showString " "
            . liftShowsPrec showsPrecA showListA nextPrec fa

instance (Comonad w) => Comonad (EnvT e w) where
  duplicate (EnvT e wa) = EnvT e (extend (EnvT e) wa)
  extract (EnvT _ wa) = extract wa

instance HFunctor (EnvT e) where
  hmap nat (EnvT e wa) = EnvT e $ nat wa

runEnvT :: EnvT w f a -> Pair w (f a)
runEnvT (EnvT w fa) = w :!: fa

-- | A strict free applicative pattern functor.
data FreeF f a b
  = Pure a
  | Free (f b)
  deriving stock
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type instance Strict FreeF = 'True

type instance Strict (FreeF _f) = 'True

type instance Strict (FreeF _f _a) = 'True

type instance Strict (FreeF _f _a _b) = 'True

instance (Eq1 f, Eq a) => Eq1 (FreeF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f) => Eq2 (FreeF f) where
  liftEq2 eqA eqB = curry \case
    Pure a :!: Pure a' -> eqA a a'
    Free fb :!: Free fb' -> liftEq eqB fb fb'
    _ :!: _ -> False

instance (Ord1 f, Ord a) => Ord1 (FreeF f a) where
  liftCompare = liftCompare2 compare

instance (Ord1 f) => Ord2 (FreeF f) where
  liftCompare2 compareA compareB = curry \case
    Pure a :!: Pure a' -> compareA a a'
    Pure _ :!: Free _ -> LT
    Free _ :!: Pure _ -> GT
    Free fb :!: Free fb' -> liftCompare compareB fb fb'

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

-- | `Data.Strict.unzip` is just the wrong thing.
unzip :: (Functor f) => f (Pair a b) -> Pair (f a) (f b)
unzip x = fst <$> x :!: snd <$> x
