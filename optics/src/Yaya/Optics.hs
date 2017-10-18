module Yaya.Optics where

import Control.Arrow
import Control.Lens
import Data.Either.Combinators

import Yaya.Control

type BialgebraIso f a = Iso' (f a) a
type AlgebraPrism f a = Prism' (f a) a
type CoalgebraPrism f a = Prism' a (f a)

cursiveIso :: Cursive t f => BialgebraIso f t
cursiveIso = iso embed project

birecursiveIso
  :: (Recursive t f, Corecursive t f)
  => BialgebraIso f a
  -> Iso' t a
birecursiveIso alg = iso (cata (view alg)) (ana (review alg))

recursivePrism
  :: (Recursive t f, Corecursive t f, Traversable f)
  => AlgebraPrism f a
  -> Prism' t a
recursivePrism alg = prism (ana (review alg)) (\t -> mapLeft (const t) $ cataM (matching alg) t)
