module Yaya.Example where

import Control.Monad.Trans.Free

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Maybe
import Yaya.Tuple

naturals :: (Cursive n Maybe, Corecursive t ((,) n)) => t
naturals = flip ana zeroN $ unarySequence succN

takeAnother
  :: (Cursive t ((,) a), Cursive u (XNor a))
  => Algebra Maybe (t -> u)
takeAnother Nothing = embed . const None
takeAnother (Just f) = embed . uncurry Both . fmap f . project

takeAvailable
  :: (Cursive t (XNor a), Cursive u (XNor a))
  => Algebra Maybe (t -> u)
takeAvailable Nothing = embed . const None
takeAvailable (Just f) = embed . fmap f . project

takeUpTo
  :: (Recursive n Maybe, Cursive s (XNor a), Cursive l (XNor a))
  => n -> s -> l
takeUpTo = cata takeAvailable

take
  :: (Recursive n Maybe, Cursive s ((,) a), Cursive l (XNor a))
  => n -> s -> l
take = cata takeAnother

-- | Turns part of a structure inductive, so it can be analyzed, without forcing
--   the entire tree.
maybeReify
  :: (Cursive s f, Cursive l (FreeF f s), Functor f)
  => Algebra Maybe (s -> l)
maybeReify Nothing = embed . Pure
maybeReify (Just f) = embed . Free . fmap f . project

reifyUpTo
  :: (Recursive n Maybe, Cursive s f, Cursive l (FreeF f s), Functor f)
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

duplicate :: Coalgebra ((,) a) a
duplicate i = (i, i)

constantly :: Corecursive t ((,) a) => a -> t
constantly = ana duplicate
