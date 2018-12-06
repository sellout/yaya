module Yaya where

import Control.Arrow
import Numeric.Natural

type Algebra f a = f a -> a
type GAlgebra w f a = f (w a) -> a
type ElgotAlgebra w f a = w (f a) -> a
type AlgebraM m f a = f a -> m a
type GAlgebraM m w f a = f (w a) -> m a
type ElgotAlgebraM m w f a = w (f a) -> m a

type Coalgebra f a = a -> f a
type GCoalgebra m f a = a -> f (m a)
type ElgotCoalgebra m f a = a -> m (f a)
type CoalgebraM m f a = a -> m (f a)
type GCoalgebraM m n f a = a -> m (f (n a))

-- TODO: Redefine this using `Natural`
-- | When folded, returns the height of the data structure.
height :: Foldable f => Algebra f Integer
height = (+ 1) . foldr max (-1)

-- NB: It seems like this could be some more general notion of this, like
--        (Foldable f, Semiring a) => Algebra f a
-- | When folded, returns the nembur ef nodes in the data structure.
size :: Foldable f => Algebra f Natural
size = foldr (+) 1

-- | Combines two `Algebra`s with different carriers into a single tupled
--  `Algebra`.
zipAlgebras :: Functor f => Algebra f a -> Algebra f b -> Algebra f (a, b)
zipAlgebras f g = (f . fmap fst &&& g . fmap snd)
