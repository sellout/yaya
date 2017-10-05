module Yaya.Example where

import Yaya
import Yaya.Control

unarySequence :: (a -> a) -> Coalgebra ((,) a) a
unarySequence f a = (a, f a)

-- naturals :: (Corecursive n Maybe, Corecursive t ((,) n)) => t
-- naturals = unarySequence succ zero

binarySequence :: (a -> a -> a) -> Coalgebra ((,) a) (a, a)
binarySequence f (a, b) = (a, (b, f a b))

ternarySequence :: (a -> a -> a -> a) -> Coalgebra ((,) a) (a, a, a)
ternarySequence f (a, b, c) = (a, (b, c, f a b c))

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

-- | Lops off the branches of the tree below a certain depth, turning a
--   potentially-infinite structure into a finite one. Like a generalized
--  'take'.
-- truncate :: (Corecursive t f, Recursive u (CoEnv t f)) => Int -> t -> u
-- truncate i = ana (\(i, nu) ->
--                      if i <= 0
--                        then CoEnv $ Left nu
--                        else CoEnv . Right . fmap (i - 1,) $ project nu)
--                  . (i,)
