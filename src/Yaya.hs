module Yaya where

type Algebra f a = f a -> a
type GAlgebra w f a = f (w a) -> a
type AlgebraM m f a = f a -> m a
type GAlgebraM m w f a = f (w a) -> m a

type Coalgebra f a = a -> f a
type GCoalgebra m f a = a -> f (m a)
type CoalgebraM m f a = a -> m (f a)
type GCoalgebraM m n f a = a -> m (f (n a))
