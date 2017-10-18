module Yaya.Unsafe.Optics where

import Control.Arrow
import Control.Lens
import Data.Either.Combinators

import Yaya.Control
import Yaya.Optics
import Yaya.Unsafe.Control

corecursivePrism
  :: (Cursive t f, Recursive t f, Corecursive t f, Traversable f)
  => CoalgebraPrism f a
  -> Prism' a t
corecursivePrism alg = prism (cata (review alg)) (anaM (matching alg))
