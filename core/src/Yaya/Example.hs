module Yaya.Example where

import Control.Monad.Trans.Free
import Data.Functor.Day

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Maybe
import Yaya.Tuple

naturals :: (Steppable n Maybe, Corecursive t ((,) n)) => t
naturals = ana (unarySequence succN) zeroN

takeAnother :: Day Maybe ((,) a) b -> XNor a b
takeAnother = \case
  Day Nothing  _      _ -> Neither
  Day (Just x) (h, t) f -> Both h (f x t)

takeAvailable :: Day Maybe (XNor a) b -> XNor a b
takeAvailable = \case
  Day Nothing  _ _ -> Neither
  Day (Just x) t f -> fmap (f x) t

-- | Extracts _no more than_ `n` elements from the possibly-infinite sequence
--  `s`.
takeUpTo
  :: (Recursive n Maybe, Steppable s (XNor a), Steppable l (XNor a))
  => n -> s -> l
takeUpTo = cata (lowerDay (embed . takeAvailable))

-- | Extracts _exactly_ `n` elements from the infinite stream `s`.
take
  :: (Recursive n Maybe, Steppable s ((,) a), Steppable l (XNor a))
  => n -> s -> l
take = cata (lowerDay (embed . takeAnother))

-- | Turns part of a structure inductive, so it can be analyzed, without forcing
--   the entire tree.
maybeReify
  :: (Steppable s f, Steppable l (FreeF f s), Functor f)
  => Algebra Maybe (s -> l)
maybeReify Nothing = embed . Pure
maybeReify (Just f) = embed . Free . fmap f . project

reifyUpTo
  :: (Recursive n Maybe, Steppable s f, Steppable l (FreeF f s), Functor f)
  => n -> s -> l
reifyUpTo = cata maybeReify

fibonacciPolynomials :: (Integral i, Corecursive t ((,) i)) => i -> t
fibonacciPolynomials x = lucasSequenceU x (-1)

fibonacci :: Corecursive t ((,) Int) => t
fibonacci = fibonacciPolynomials 1

lucasSequence' :: Integral i => i -> i -> Coalgebra ((,) i) (i, i)
lucasSequence' p q = binarySequence (\n2 n1 -> p * n1 - q * n2)

lucasSequenceU :: (Integral i, Corecursive t ((,) i)) => i -> i -> t
lucasSequenceU p q = lucasSequence' p q `ana` (0, 1)

lucasSequenceV :: (Integral i, Corecursive t ((,) i)) => i -> i -> t
lucasSequenceV p q = lucasSequence' p q `ana` (2, p)

lucas :: Integral i => Corecursive t ((,) i) => t
lucas = lucasSequenceV 1 (-1)

pell :: (Integral i, Corecursive t ((,) i)) => t
pell = lucasSequenceU 2 (-1)

jacobsthal :: (Integral i, Corecursive t ((,) i)) => t
jacobsthal = lucasSequenceU 1 (-2)

mersenne :: (Integral i, Corecursive t ((,) i)) => t
mersenne = lucasSequenceU 3 2

-- | Creates an infinite stream of the provided value.
constantly :: Corecursive t ((,) a) => a -> t
constantly = ana duplicate

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  'take'.
truncate
  :: (Recursive n Maybe, Steppable t f, Steppable u (FreeF f ()), Functor f)
  => n -> t -> u
truncate = cata (lowerDay (embed . truncate'))

truncate' :: Functor f => Day Maybe f a -> FreeF f () a
truncate' = \case
  Day Nothing  fa _ -> Pure ()
  Day (Just n) fa f -> Free (fmap (f n) fa)
