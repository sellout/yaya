-- | Contains all the commonly-named folds that aren’t core to the library. In
--   general, this can be seen as a mapping from names you may have heard or
--   read in a paper to how Yaya expects you to achieve the same end. Of course,
--   you can always import this module and use the “common” name as well.
module Yaya.Zoo where

import Control.Arrow hiding (first)
import Data.Bifunctor
import Data.Bitraversable
import Data.Function
import Data.Profunctor

import Yaya
import Yaya.Control
import Yaya.Data

-- | A recursion scheme that allows you to return a complete branch when
--   unfolding.
apo
  :: (Cursive t f, Corecursive t f, Functor f)
  => GCoalgebra (Either t) f a
  -> a
  -> t
apo = gana $ distGApo project

-- | A recursion scheme that gives you access to the original structure as you
--   fold. (A specialization of 'zygo'.
para
  :: (Cursive t f, Recursive t f, Functor f)
  => GAlgebra ((,) t) f a
  -> t
  -> a
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
-- NB: This is a newtype so we can create the usual instances.
newtype Partial a = Partial { fromPartial :: Nu (Either a) }

-- TODO: There may be some way to do this over an arbitrary 'newtype', or at
--       least a way to do it over an arbitrary 'Iso'.
insidePartial :: (Nu (Either a) -> Nu (Either b)) -> Partial a -> Partial b
insidePartial f = Partial . f . fromPartial

instance Functor Partial where
  fmap f = insidePartial $ comap f

instance Applicative Partial where
  pure = Partial . embed . Left
  ff <*> fa =
    flip insidePartial ff
    $ elgotAna (distGApo project)
               ((fromPartial . flip fmap fa +++ Right) . project)

instance Monad Partial where
  pa >>= f = join $ fmap f pa
    where
      join =
        insidePartial
        $ elgotAna (distGApo project) ((fromPartial +++ Right) . project)

-- | Always-infinite streams (as opposed to 'Colist', which _may_ terminate).
type Stream a = Nu ((,) a)

-- | A more general implementation of 'fmap', because it can also work to, from,
--   or within monomorphic structures, obviating the need for classes like
--  'MonoFunctor'.
map :: (Recursive t (f a), Cursive u (f b), Bifunctor f) => (a -> b) -> t -> u
map f = cata $ embed . first f

comap
  :: (Cursive t (f a), Corecursive u (f b), Bifunctor f)
  => (a -> b)
  -> t
  -> u
comap f = ana $ first f . project

-- | A more general implementation of 'fmap', because it can also work to, from,
--   or within monomorphic structures, obviating the need for classes like
--  'MonoTraversable'.
-- TODO: Weaken the 'Monad' constraint to 'Applicative'.
traverse
  :: ( Recursive t (f a)
     , Cursive u (f b)
     , Bitraversable f
     , Traversable (f a)
     , Monad m)
  => (a -> m b)
  -> t
  -> m u
traverse f = cataM $ fmap embed . bitraverse f pure

-- | A more general implementation of 'contramap', because it can also work to,
--   from, or within monomorphic structures.
contramap
  :: (Recursive t (f b), Cursive u (f a), Profunctor f)
  => (a -> b)
  -> t
  -> u
contramap f = cata $ embed . lmap f

cocontramap
  :: (Cursive t (f b), Corecursive u (f a), Profunctor f)
  => (a -> b)
  -> t
  -> u
cocontramap f = ana $ lmap f . project
