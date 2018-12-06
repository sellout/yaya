module Yaya.Tuple where

import Yaya
import Yaya.Control

-- | Converts a single value into a tuple with the same value on both sides.
--   > x &&& y = (x *** y) . duplicate
duplicate :: Coalgebra ((,) a) a
duplicate i = (i, i)

unarySequence :: (a -> a) -> Coalgebra ((,) a) a
unarySequence f a = (a, f a)

binarySequence :: (a -> a -> a) -> Coalgebra ((,) a) (a, a)
binarySequence f (a, b) = (a, (b, f a b))

ternarySequence :: (a -> a -> a -> a) -> Coalgebra ((,) a) (a, a, a)
ternarySequence f (a, b, c) = (a, (b, c, f a b c))
