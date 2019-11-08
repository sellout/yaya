-- | This should probably be a separate library, but it provides a number of
--   functor type classes between various categories.
module Yaya.Functor where

import Data.Bifunctor

-- | A functor from the category of endofunctors to *Hask*. The @D@ is meant to
--   be a mnemonic for “down”, as we’re “lowering” from endofunctors to types.
class DFunctor (d :: (* -> *) -> *) where
  dmap :: (forall a. f a -> g a) -> d f -> d g

-- | This isn’t a Functor instance because of the position of the @a@, but you
--   can use it like:
--   > newtype List a = List (Mu (XNor a))
--   > instance Functor List where
--   >   fmap f (List mu) = List (firstMap f mu)
firstMap :: (DFunctor d, Bifunctor f) => (a -> b) -> d (f a) -> d (f b)
firstMap f = dmap (first f)

-- | An endofunctor in the category of endofunctors.
class HFunctor (h :: (* -> *) -> * -> *) where
  hmap :: (forall a. f a -> g a) -> h f a -> h g a
