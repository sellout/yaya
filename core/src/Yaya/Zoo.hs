-- | Contains all the commonly-named folds that aren’t core to the library.
module Yaya.Zoo where

import Control.Comonad.Cofree (Cofree(..))

import Yaya
import Yaya.Control

apo :: (Cursive t f, Corecursive t f, Functor f) => GCoalgebra (Either t) f a -> a -> t
apo = gana $ distGApo project

para :: (Cursive t f, Recursive t f, Functor f) => GAlgebra ((,) t) f a -> t -> a
para = gcata $ distZygo embed

zygo
  :: (Recursive t f, Functor f)
  => Algebra f b
  -> GAlgebra ((,) b) f a
  -> t
  -> a
zygo φ = gcata $ distZygo φ
