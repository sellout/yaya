module Yaya.Unsafe.Zoo where

import Control.Comonad.Cofree
import Control.Monad.Trans.Free

import Yaya
import Yaya.Control
import Yaya.Unsafe.Control
import Yaya.Unsafe.Data

chrono :: Functor f => GAlgebra (Cofree f) f b -> GCoalgebra (Free f) f a -> a -> b
chrono = ghylo (distGHisto id) (distGFutu id)

-- codyna :: Functor f => Algebra f b -> GCoalgebra (Free f) f a -> a -> b
-- codyna = ghylo distCata $ distGFutu id

-- dyna :: Functor f => GAlgebra (Cofree f) f b -> Coalgebra f a -> a -> b
-- dyna = flip ghylo distAna $ distGHisto id

futu :: (Corecursive t f, Functor f) => GCoalgebra (Free f) f a -> a -> t
futu = gana $ distGFutu id

histo :: (Recursive t f, Functor f) => GAlgebra (Cofree f) f a -> t -> a
histo = gcata $ distGHisto id
