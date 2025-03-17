{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

module Yaya.Applied
  ( Void,
    absurd,
    append,
    at,
    atMay,
    coappend,
    constantly,
    defaultMconcat,
    defaultSconcat,
    defaultStimes,
    drop',
    drop,
    fibonacci,
    fibonacciPolynomials,
    foldr',
    foldr1,
    fromList,
    fromMaybe,
    height,
    increment,
    incrementNatural,
    jacobsthal,
    length,
    lucas,
    lucasSequenceU,
    lucasSequenceV,
    maybeReify,
    mersenne,
    naturals,
    now,
    pell,
    reifyUpTo,
    reverse',
    reverse,
    runToEnd,
    stimes',
    stimes'',
    succN,
    tail,
    take,
    takeUpTo,
    toPositive,
    truncate,
    vacuous,
    zeroN,

    -- * numeric constants
    n0,
    n1,
    n2,
    n3,
    n4,
    n5,
    n6,
    n7,
    n8,
    n9,
    n10,
    p1,
    p2,
    p3,
    p4,
    p5,
    p6,
    p7,
    p8,
    p9,
    p10,
  )
where

import safe "base" Control.Category (id, (.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Foldable (Foldable, foldr)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Functor.Identity (Identity, runIdentity)
import safe "base" Data.Int (Int)
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord, max)
import safe "base" Data.Semigroup (Semigroup, (<>))
import safe "free" Control.Monad.Trans.Free (FreeF (Free, Pure))
import safe "this" Yaya.Fold
  ( Algebra,
    Corecursive,
    Mu,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    cata2,
    embed,
    project,
    pattern Embed,
  )
import safe "this" Yaya.Fold.Common
  ( diagonal,
    fromEither,
    lucasSequence',
    maybeTakeNext,
    never,
    replaceNeither,
    takeAnother,
    takeAvailable,
    takeNext,
    toRight,
    truncate',
    unarySequence,
  )
import safe "this" Yaya.Pattern
  ( AndMaybe,
    Either (Left),
    Log2 (Double, DoublePlus, One),
    Maybe (Just, Nothing),
    Pair,
    XNor (Both, Neither),
    andMaybe,
    maybe,
    xnor,
  )
import safe "base" Prelude (Integral)

now :: (Steppable (->) t (Either a)) => a -> t
now = embed . Left

-- | This will collapse all the intermediate steps to get to the value that must
--   exist at the end.
runToEnd :: (Recursive (->) t (Either a)) => t -> a
runToEnd = cata fromEither

-- | Converts exceptional divergence to non-termination.
fromMaybe ::
  (Steppable (->) t (Either a), Corecursive (->) t (Either a)) => Maybe a -> t
fromMaybe = maybe (ana (toRight . never) ()) now

type Void = Mu Identity

absurd :: (Recursive (->) t Identity) => t -> a
absurd = cata runIdentity

vacuous :: (Functor f, Recursive (->) t Identity) => f t -> f a
vacuous = fmap absurd

zeroN :: (Steppable (->) t Maybe) => t
zeroN = embed Nothing

succN :: (Steppable (->) t Maybe) => t -> t
succN = embed . Just

n0 :: (Steppable (->) t Maybe) => t
n0 = zeroN

n1 :: (Steppable (->) t Maybe) => t
n1 = succN n0

n2 :: (Steppable (->) t Maybe) => t
n2 = succN n1

n3 :: (Steppable (->) t Maybe) => t
n3 = succN n2

n4 :: (Steppable (->) t Maybe) => t
n4 = succN n3

n5 :: (Steppable (->) t Maybe) => t
n5 = succN n4

n6 :: (Steppable (->) t Maybe) => t
n6 = succN n5

n7 :: (Steppable (->) t Maybe) => t
n7 = succN n6

n8 :: (Steppable (->) t Maybe) => t
n8 = succN n7

n9 :: (Steppable (->) t Maybe) => t
n9 = succN n8

n10 :: (Steppable (->) t Maybe) => t
n10 = succN n9

p1 :: (Steppable (->) t Log2) => t
p1 = embed One

p2 :: (Steppable (->) t Log2) => t
p2 = embed $ Double p1

p3 :: (Steppable (->) t Log2) => t
p3 = embed $ DoublePlus p1

p4 :: (Steppable (->) t Log2) => t
p4 = embed $ Double p2

p5 :: (Steppable (->) t Log2) => t
p5 = embed $ DoublePlus p2

p6 :: (Steppable (->) t Log2) => t
p6 = embed $ Double p3

p7 :: (Steppable (->) t Log2) => t
p7 = embed $ DoublePlus p3

p8 :: (Steppable (->) t Log2) => t
p8 = embed $ Double p4

p9 :: (Steppable (->) t Log2) => t
p9 = embed $ DoublePlus p4

p10 :: (Steppable (->) t Log2) => t
p10 = embed $ Double p5

height :: (Foldable f, Steppable (->) n Maybe, Ord n) => f n -> n
height = foldr (max . succN) zeroN

naturals :: (Steppable (->) n Maybe, Corecursive (->) t ((,) n)) => t
naturals = ana (unarySequence succN) zeroN

length :: (Recursive (->) t (XNor a), Steppable (->) n Maybe, Ord n) => t -> n
length = cata height

append :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u -> u
append front back = cata (embed . replaceNeither (project back)) front

coappend ::
  (Projectable (->) t (XNor a), Corecursive (->) u (XNor a)) => t -> t -> u
coappend front back = ana (replaceNeither (project back) . project) front

foldr' :: (Recursive (->) t (XNor a)) => (a -> b -> b) -> b -> t -> b
foldr' op = cata . flip xnor op

foldr1 :: (Recursive (->) t (AndMaybe a)) => (a -> a -> a) -> t -> a
foldr1 = cata . andMaybe id

-- | This is a recursion-free implementation of `sconcat`. However, the
--   specialization to `NonEmpty` for `Semigroup` will cause it to use `Native`
--   recursion.
defaultSconcat :: (Semigroup a, Recursive (->) t (AndMaybe a)) => t -> a
defaultSconcat = foldr1 (<>)

defaultMconcat :: (Monoid a, Recursive (->) t (XNor a)) => t -> a
defaultMconcat = foldr' (<>) mempty

-- | A total variant of `stimes` that doesn’t allow @0@ times.
defaultStimes :: (Semigroup a, Recursive (->) p Log2) => p -> a -> a
defaultStimes = flip $ stimes' (<>)

stimes'' :: (a -> a -> a) -> a -> Log2 a -> a
stimes'' f x = \case
  One -> x
  Double x' -> f x' x'
  DoublePlus x' -> f x (f x' x')

stimes' :: (Recursive (->) p Log2) => (a -> a -> a) -> a -> p -> a
stimes' f = cata . stimes'' f

-- TODO: Move these instances to `Native`, because they rely on folding a `NonEmpty` for `sconcat`.
-- NB: Translating `stimes` to recursion schemes could be interesting, because it does a logarithmic number of applications. E.g., I can imagine folding the Nat to a structure that represents doublings and increments somehow.

-- | Converts a natural number to a positive number, returning `Nothing` if the
--   input is zero.
toPositive ::
  forall p n.
  ( Projectable (->) n Maybe,
    Recursive (->) n Maybe,
    Recursive (->) p Log2,
    Steppable (->) p Log2
  ) =>
  n ->
  Maybe p
toPositive = fmap (cata $ embed . incrementNatural) . project

-- | Infallibly converts a natural number to a positive number by adding one.
incrementNatural :: (Recursive (->) p Log2, Steppable (->) p Log2) => Maybe p -> Log2 p
incrementNatural = maybe One increment

-- | Increment the positive number /iff/ the `Bool` argument is `True`.
--
--  __NB__: This is a slightly awkward algebra that probably only make sense in
--          the context of `increment`.
maybeIncrement ::
  (Steppable (->) p Log2) => Bool -> Log2 (Bool -> Log2 p) -> Log2 p
maybeIncrement incp =
  if incp
    then \case
      One -> Double $ embed One
      Double pf -> DoublePlus . embed $ pf False
      DoublePlus pf -> Double . embed $ pf True
    else fmap $ embed . ($ False)

-- | Increment a positive number.
increment :: (Recursive (->) p Log2, Steppable (->) p Log2) => p -> Log2 p
increment = flip (cata $ flip maybeIncrement) True

-- |
--
--  __NB__: This can’t be implemented using `Day`, because we need to know that
--          the `XNor` contains the fixed-point. And even if we could, it would
--          require `Steppable` rather than just `Projectable`.
drop' :: (Projectable (->) t (XNor a)) => Maybe (t -> t) -> t -> t
drop' (Just fn) (Embed (Both _ t)) = fn t
drop' _ t = t

drop :: (Recursive (->) n Maybe, Projectable (->) t (XNor a)) => n -> t -> t
drop = cata drop'

tail :: (Projectable (->) t (XNor a)) => t -> t
tail = drop (embed . Just $ embed Nothing :: Mu Maybe)

reverse' ::
  (Steppable (->) t (XNor a)) =>
  XNor a (XNor a t -> XNor a t) ->
  XNor a t ->
  XNor a t
reverse' Neither = id
reverse' (Both a fn) = fn . Both a . embed

reverse :: (Recursive (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u
reverse = embed . flip (cata reverse') Neither

-- | Extracts _no more than_ @n@ elements from the possibly-infinite sequence
--  @s@.
takeUpTo ::
  ( Recursive (->) n Maybe,
    Projectable (->) s (XNor a),
    Steppable (->) l (XNor a)
  ) =>
  n ->
  s ->
  l
takeUpTo = cata2 (embed . takeAvailable)

-- | Extracts _exactly_ @n@ elements from the infinite stream @s@.
take ::
  ( Recursive (->) n Maybe,
    Projectable (->) s ((,) a),
    Steppable (->) l (XNor a)
  ) =>
  n ->
  s ->
  l
take = cata2 (embed . takeAnother)

-- | Extracts the element at a finite index of an infinite sequence (a
--  `Data.List.!!` that can't fail).
at :: (Recursive (->) n Maybe, Projectable (->) s ((,) a)) => n -> s -> a
at = cata2 takeNext

-- | Extracts the element at a finite index of a (co)list (a `Data.List.!!` that
--   fails with `Nothing`).
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
  ( Recursive (->) n Maybe,
    Projectable (->) s f,
    Steppable (->) l (FreeF f s),
    Functor f
  ) =>
  n ->
  s ->
  l
reifyUpTo = cata maybeReify

fibonacciPolynomials :: (Integral i, Corecursive (->) t ((,) i)) => i -> t
fibonacciPolynomials x = lucasSequenceU x (-1)

fibonacci :: (Corecursive (->) t ((,) Int)) => t
fibonacci = fibonacciPolynomials 1

lucasSequenceU :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceU p q = lucasSequence' p q `ana` (0, 1)

lucasSequenceV :: (Integral i, Corecursive (->) t ((,) i)) => i -> i -> t
lucasSequenceV p q = lucasSequence' p q `ana` (2, p)

lucas :: (Integral i) => (Corecursive (->) t ((,) i)) => t
lucas = lucasSequenceV 1 (-1)

pell :: (Integral i, Corecursive (->) t ((,) i)) => t
pell = lucasSequenceU 2 (-1)

jacobsthal :: (Integral i, Corecursive (->) t ((,) i)) => t
jacobsthal = lucasSequenceU 1 (-2)

mersenne :: (Integral i, Corecursive (->) t ((,) i)) => t
mersenne = lucasSequenceU 3 2

-- | Creates an infinite stream of the provided value.
constantly :: (Corecursive (->) t (Pair a)) => a -> t
constantly = ana diagonal

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  `Yaya.Applied.take`.
truncate ::
  ( Recursive (->) n Maybe,
    Projectable (->) t f,
    Steppable (->) u (FreeF f ()),
    Functor f
  ) =>
  n ->
  t ->
  u
truncate = cata2 (embed . truncate')

-- | An implementation of `IsList.toList` for `Corecursive` fixed-points of
--  `XNor`.
fromList :: (Corecursive (->) t (XNor a)) => [a] -> t
fromList = ana project
