{-# LANGUAGE Safe #-}

-- | This should probably be a separate library, but it provides a number of
--   functor type classes between various categories.
module Yaya.Functor where

import "base" Control.Category (Category ((.)))
import "base" Data.Bifunctor (Bifunctor, first)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Compose (Compose (Compose))
import "base" Data.Functor.Product (Product (Pair))
import "base" Data.Kind (Type)
import "transformers" Control.Applicative.Backwards (Backwards (Backwards))
import "transformers" Control.Applicative.Lift (Lift (Other, Pure))
import qualified "transformers" Control.Monad.Trans.Except as Ex
import qualified "transformers" Control.Monad.Trans.Identity as I
import qualified "transformers" Control.Monad.Trans.Maybe as M
import qualified "transformers" Control.Monad.Trans.RWS.Lazy as RWS
import qualified "transformers" Control.Monad.Trans.RWS.Strict as RWS'
import qualified "transformers" Control.Monad.Trans.Reader as R
import qualified "transformers" Control.Monad.Trans.State.Lazy as S
import qualified "transformers" Control.Monad.Trans.State.Strict as S'
import qualified "transformers" Control.Monad.Trans.Writer.Lazy as W'
import qualified "transformers" Control.Monad.Trans.Writer.Strict as W

-- | A functor from the category of endofunctors to *Hask*. The @D@ is meant to
--   be a mnemonic for “down”, as we’re “lowering” from endofunctors to types.
class DFunctor (d :: (Type -> Type) -> Type) where
  dmap :: (forall x. f x -> g x) -> d f -> d g

-- | This isn’t a Functor instance because of the position of the @a@, but you
--   can use it like:
--   > newtype List a = List (Mu (XNor a))
--   > instance Functor List where
--   >   fmap f (List mu) = List (firstMap f mu)
firstMap :: (DFunctor d, Bifunctor f) => (a -> b) -> d (f a) -> d (f b)
firstMap f = dmap (first f)

-- | An endofunctor in the category of endofunctors.
--
--  __NB__: This is similar to `Control.Monad.Morph.MFunctor` /
--         `Control.Monad.Morph.hoist` from mmorph, but without the
--         `Control.Monad.Monad` constraint on @f@.
class HFunctor (h :: (Type -> Type) -> Type -> Type) where
  hmap :: (forall x. f x -> g x) -> h f a -> h g a

instance HFunctor (Ex.ExceptT e) where
  hmap nat m = Ex.ExceptT (nat (Ex.runExceptT m))

instance HFunctor I.IdentityT where
  hmap nat m = I.IdentityT (nat (I.runIdentityT m))

instance HFunctor M.MaybeT where
  hmap nat m = M.MaybeT (nat (M.runMaybeT m))

instance HFunctor (R.ReaderT r) where
  hmap nat m = R.ReaderT $ nat . R.runReaderT m

instance HFunctor (RWS.RWST r w s) where
  hmap nat m = RWS.RWST (\r s -> nat (RWS.runRWST m r s))

instance HFunctor (RWS'.RWST r w s) where
  hmap nat m = RWS'.RWST (\r s -> nat (RWS'.runRWST m r s))

instance HFunctor (S.StateT s) where
  hmap nat m = S.StateT $ nat . S.runStateT m

instance HFunctor (S'.StateT s) where
  hmap nat m = S'.StateT $ nat . S'.runStateT m

instance HFunctor (W.WriterT w) where
  hmap nat m = W.WriterT (nat (W.runWriterT m))

instance HFunctor (W'.WriterT w) where
  hmap nat m = W'.WriterT (nat (W'.runWriterT m))

instance (Functor f) => HFunctor (Compose f) where
  hmap nat (Compose f) = Compose (fmap nat f)

instance HFunctor (Product f) where
  hmap nat (Pair f g) = Pair f (nat g)

instance HFunctor Backwards where
  hmap nat (Backwards f) = Backwards (nat f)

instance HFunctor Lift where
  hmap _ (Pure a) = Pure a
  hmap nat (Other f) = Other (nat f)
