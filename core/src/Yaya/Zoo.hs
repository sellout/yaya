-- | Contains all the commonly-named folds that aren’t core to the library.
module Yaya.Zoo where

import Yaya
import Yaya.Control
import Yaya.Data

-- | A recursion scheme that allows you to return a complete branch when
--   unfolding.
apo :: (Cursive t f, Corecursive t f, Functor f) => GCoalgebra (Either t) f a -> a -> t
apo = gana $ distGApo project

-- | A recursion scheme that gives you access to the original structure as you
--   fold. (A specialization of 'zygo'.
para :: (Cursive t f, Recursive t f, Functor f) => GAlgebra ((,) t) f a -> t -> a
para = gcata $ distZygo embed

-- | A recursion scheme that uses a “helper algebra” to provide additional
--   information when folding. (A generalization of 'para', and specialization
--   of 'mutu'.)
zygo
  :: (Recursive t f, Functor f)
  => Algebra f b
  -> GAlgebra ((,) b) f a
  -> t
  -> a
zygo φ = gcata $ distZygo φ

-- | Potentially-infinite lists, like 'Data.List'.
type Colist a = Nu (XNor a)

-- | Finite lists.
type List a = Mu (XNor a)

-- | Finite non-empty lists.
type NonEmptyList a = Mu (AndMaybe a)

-- | Finite natural numbers.
type Nat = Mu Maybe

-- | Represents partial functions that may eventually return a value ('Left').
type Partial a = Nu (Either a)

-- | Always-infinite streams (as opposed to 'Colist', which _may_ terminate).
type Stream a = Nu ((,) a)
