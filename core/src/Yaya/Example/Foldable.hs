-- | This shows how 'Data.Foldable' is basically 'Recursive' specialized to
--   lists. The true operation of 'Data.Foldable' is 'toList'.
--
--   As these few operations have the usual signatures, the rest of the type
--   class can be implemented in the as in 'base'.
module Yaya.Example.Foldable where

import Control.Monad.Trans.Free

import Yaya
import Yaya.Control
import Yaya.Data

lowerMonoid :: Monoid m => (a -> m) -> Algebra (XNor a) m
lowerMonoid f = \case
  Neither  -> mempty
  Both a b -> mappend (f a) b

foldMap :: (Recursive t (XNor a), Monoid m) => (a -> m) -> t -> m
foldMap = cata . lowerMonoid

unFree :: Steppable t f => Algebra (FreeF f t) t
unFree = \case
  Pure t  -> t
  Free ft -> embed ft

-- | This class represents the ability of a structure to be converted to a
--   list. It is equivalent to `Foldable`, but designed to illustrate the
--   representation of `Foldable` as `Recursive` specialized to lists.
class Listable f where
  naturalList :: f a b -> Free (XNor a) b
  -- toColist :: (Steppable t (f a), Corecursive u (XNor a)) => t -> u
  -- toColist = elgotAna seqFree (naturalList . project)
  -- toList :: (Recursive t (f a), Steppable u (XNor a)) => t -> u
  -- toList = cata (embed . unFree . naturalList)

-- FIXME: Use `cata . liftCoEnv`  instead of `iter`.

-- | This is simply `cata` applied to a list – the function is the `Cons`
--   case, while the initial value is the `Nil` case.
foldr :: (Listable f, Recursive t (f a)) => (a -> b -> b) -> b -> t -> b
foldr f b =
  cata (iter (\case
                 Neither  -> b
                 Both a r -> f a r)
        . naturalList)

-- | Simply 'cata' with a carrier of 'b -> b'.
foldl :: (Listable f, Recursive t (f a)) => (b -> a -> b) -> b -> t -> b
foldl f =
  flip
  (cata (iter (\case
                  Neither  -> id
                  Both a g -> g . flip f a)
         . naturalList))
