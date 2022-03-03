module Yaya.Applied where

import Control.Monad.Trans.Free
import Data.Functor.Identity
import Yaya.Fold
import Yaya.Fold.Common
import Yaya.Pattern

now :: Steppable (->) t (Either a) => a -> t
now = embed . Left

-- | This will collapse all the intermediate steps to get to the value that must
--   exist at the end.
runToEnd :: Recursive (->) t (Either a) => t -> a
runToEnd = cata fromEither

-- | Converts exceptional divergence to non-termination.
fromMaybe :: (Steppable (->) t (Either a), Corecursive (->) t (Either a)) => Maybe a -> t
fromMaybe = maybe (ana (toRight . never) ()) now

type Void = Mu Identity

absurd :: Recursive (->) t Identity => t -> a
absurd = cata runIdentity

vacuous :: (Functor f, Recursive (->) t Identity) => f t -> f a
vacuous = fmap absurd

zeroN :: Steppable (->) t Maybe => t
zeroN = embed Nothing

succN :: Steppable (->) t Maybe => t -> t
succN = embed . Just

height :: (Foldable f, Steppable (->) n Maybe, Ord n) => f n -> n
height = foldr (max . succN) zeroN

naturals :: (Steppable (->) n Maybe, Corecursive (->) t ((,) n)) => t
naturals = ana (unarySequence succN) zeroN

-- | Extracts _no more than_ @n@ elements from the possibly-infinite sequence
--  @s@.
takeUpTo ::
  (Recursive (->) n Maybe, Projectable (->) s (XNor a), Steppable (->) l (XNor a)) =>
  n ->
  s ->
  l
takeUpTo = cata2 (embed . takeAvailable)

-- | Extracts _exactly_ @n@ elements from the infinite stream @s@.
take ::
  (Recursive (->) n Maybe, Projectable (->) s ((,) a), Steppable (->) l (XNor a)) =>
  n ->
  s ->
  l
take = cata2 (embed . takeAnother)

-- | Extracts the element at a finite index of an infinite sequence (a `!!` that
--   can't fail).
at :: (Recursive (->) n Maybe, Projectable (->) s ((,) a)) => n -> s -> a
at = cata2 takeNext

-- | Extracts the element at a finite index of a (co)list (a `!!` that fails
--   with `Nothing`).
atMay ::
  (Recursive (->) n Maybe, Projectable (->) s (XNor a)) => n -> s -> Maybe a
atMay = cata2 maybeTakeNext

-- | Turns part of a structure inductive, so it can be analyzed, without forcing
--   the entire tree.
maybeReify ::
  (Projectable (->) s f, Steppable (->) l (FreeF f s), Functor f) =>
  Algebra (->) Maybe (s -> l)
maybeReify Nothing = embed . Pure
maybeReify (Just f) = embed . Free . fmap f . project

reifyUpTo ::
  (Recursive (->) n Maybe, Projectable (->) s f, Steppable (->) l (FreeF f s), Functor f) =>
  n ->
  s ->
  l
reifyUpTo = cata maybeReify

fibonacciPolynomials :: (Integral i, Corecursive (->) t ((,) i)) => i -> t
fibonacciPolynomials x = lucasSequenceU x (-1)

fibonacci :: Corecursive (->) t ((,) Int) => t
fibonacci = fibonacciPolynomials 1

lucasSequenceU :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceU p q = lucasSequence' p q `ana` (0, 1)

lucasSequenceV :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceV p q = lucasSequence' p q `ana` (2, p)

lucas :: Integral i => Corecursive (->) t ((,) i) => t
lucas = lucasSequenceV 1 (-1)

pell :: (Integral i, Corecursive (->) t ((,) i)) => t
pell = lucasSequenceU 2 (-1)

jacobsthal :: (Integral i, Corecursive (->) t ((,) i)) => t
jacobsthal = lucasSequenceU 1 (-2)

mersenne :: (Integral i, Corecursive (->) t ((,) i)) => t
mersenne = lucasSequenceU 3 2

-- | Creates an infinite stream of the provided value.
constantly :: Corecursive (->) t ((,) a) => a -> t
constantly = ana diagonal

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  `Yaya.Applied.take`.
truncate ::
  (Recursive (->) n Maybe, Projectable (->) t f, Steppable (->) u (FreeF f ()), Functor f) =>
  n ->
  t ->
  u
truncate = cata2 (embed . truncate')
