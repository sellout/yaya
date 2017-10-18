-- | This shows how 'Data.Foldable' is basically 'Recursive' specialized to
--   lists. The true operation of 'Data.Foldable' is 'toList'.
--
--   As these few operations have the usual signatures, the rest of the type
--   class can be implemented in the as in 'base'.
module Yaya.Example.Foldable where

import Yaya.Control
import Yaya.Data

foldMap :: (Recursive t (XNor a), Monoid m) => (a -> m) -> t -> m
foldMap f =
  cata (\case
           None     -> mempty
           Both a b -> mappend (f a) b)

foldr :: Recursive t (XNor a) => (a -> b -> b) -> b -> t -> b
foldr f b =
  cata (\case
           None     -> b
           Both a b -> f a b)

-- | Simply 'cata' with a carrier of 'b -> b'.
foldl :: Recursive t (XNor a) => (b -> a -> b) -> b -> t -> b
foldl f =
  flip $ cata (\case
                  None     -> id
                  Both a g -> g . flip f a)
