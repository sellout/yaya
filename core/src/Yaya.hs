module Yaya where

import Numeric.Natural

type Algebra f a = f a -> a
type GAlgebra w f a = f (w a) -> a
type ElgotAlgebra w f a = w (f a) -> a
type AlgebraM m f a = f a -> m a
type GAlgebraM m w f a = f (w a) -> m a

type Coalgebra f a = a -> f a
type GCoalgebra m f a = a -> f (m a)
type ElgotCoalgebra m f a = a -> m (f a)
type CoalgebraM m f a = a -> m (f a)
type GCoalgebraM m n f a = a -> m (f (n a))

-- TODO: Redefine this using `Natural`
height :: Foldable f => Algebra f Integer
height = (+ 1) . foldr max (-1)

size :: Foldable f => Algebra f Natural
size = foldr (+) 1
