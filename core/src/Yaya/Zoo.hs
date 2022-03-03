-- | Contains all the commonly-named folds that aren’t core to the library. In
--   general, this can be seen as a mapping from names you may have heard or
--   read in a paper to how Yaya expects you to achieve the same end. Of course,
--   you can always import this module and use the “common” name as well.
module Yaya.Zoo where

import Control.Arrow hiding (first)
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Either.Combinators
import Data.Profunctor
import Data.Tuple
import Yaya.Fold
import Yaya.Fold.Native (distCofreeT)
import Yaya.Pattern

-- | A recursion scheme that allows you to return a complete branch when
--   unfolding.
apo ::
  (Projectable (->) t f, Corecursive (->) t f, Functor f) =>
  GCoalgebra (->) (Either t) f a ->
  a ->
  t
apo = gana (seqEither project)

-- | If you have a monadic algebra, you can fold it by distributing the monad
--   over the algebra.
cataM :: (Monad m, Recursive (->) t f, Traversable f) => AlgebraM (->) m f a -> t -> m a
cataM φ = cata (φ <=< sequenceA)

-- | A recursion scheme that allows to algebras to see each others’ results. (A
--   generalization of `zygo`.) This is an example that falls outside the scope
--   of “comonadic folds”, but _would_ be covered by “adjoint folds”.
mutu ::
  (Recursive (->) t f, Functor f) =>
  GAlgebra (->) ((,) a) f b ->
  GAlgebra (->) ((,) b) f a ->
  t ->
  a
mutu φ' φ = extract . cata (φ' . fmap swap &&& φ)

gmutu ::
  (Comonad w, Comonad v, Recursive (->) t f, Functor f) =>
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) f v ->
  GAlgebra (->) (EnvT a w) f b ->
  GAlgebra (->) (EnvT b v) f a ->
  t ->
  a
gmutu w v φ' φ = extract . mutu (lowerEnv w φ') (lowerEnv v φ)
  where
    lowerEnv x φ'' =
      fmap φ''
        . x
        . fmap (fmap (uncurry EnvT) . distProd . (extract *** duplicate))
    distProd p =
      let a = fst p
       in fmap (a,) (snd p)

-- | This could use a better name.
comutu ::
  (Corecursive (->) t f, Functor f) =>
  GCoalgebra (->) (Either a) f b ->
  GCoalgebra (->) (Either b) f a ->
  a ->
  t
comutu ψ' ψ = ana (fmap swapEither . ψ' ||| ψ) . pure

-- gcomutu
--   :: (Monad m, Monad n, Corecursive (->) t f, Functor f)
--   => DistributiveLaw (->) m f
--   -> DistributiveLaw (->) n f
--   -> GCoalgebra (->) (FreeF m a) f b
--   -> GCoalgebra (->) (FreeF n b) f a
--   -> a
--   -> t
-- gcomutu m n ψ' ψ = comutu (lowerFree m ψ') (lowerFree n ψ) . pure
--   where
--     lowerFree x ψ'' =
--       fmap ((pure +++ join) . distProd . fmap (uncurry EnvT))
--       . x
--       . fmap ψ''
--     distProd :: DistributiveLaw (->) f (Either a)
--     distProd p =
--       let a = fst p
--       in fmap (\b -> (a , b)) (snd p)

mutuM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  GAlgebraM (->) m ((,) a) f b ->
  GAlgebraM (->) m ((,) b) f a ->
  t ->
  m a
mutuM φ' φ = fmap snd . cataM (bisequence . (φ' . fmap swap &&& φ))

histo :: (Recursive (->) t f, Functor f) => GAlgebra (->) (Cofree f) f a -> t -> a
histo = gcata (distCofreeT id)

-- | A recursion scheme that gives you access to the original structure as you
--   fold. (A specialization of `zygo`.)
para ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) =>
  GAlgebra (->) ((,) t) f a ->
  t ->
  a
para = gcata (distTuple embed)

-- | A recursion scheme that uses a “helper algebra” to provide additional
--   information when folding. (A generalization of `para`, and specialization
--   of `mutu`.)
zygo ::
  (Recursive (->) t f, Functor f) =>
  Algebra (->) f b ->
  GAlgebra (->) ((,) b) f a ->
  t ->
  a
zygo φ = gcata (distTuple φ)

-- | This definition is different from the one given by `gcataM (distTuple φ')`
--   because it has a monadic “helper” algebra. But at least it gives us the
--   opportunity to show how `zygo` is a specialization of `mutu`.
zygoM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f b ->
  GAlgebraM (->) m ((,) b) f a ->
  t ->
  m a
zygoM φ' = mutuM (φ' . fmap snd)

-- | Potentially-infinite lists, like `[]`.
type Colist a = Nu (XNor a)

-- | Finite lists.
type List a = Mu (XNor a)

-- | Finite non-empty lists.
type NonEmptyList a = Mu (AndMaybe a)

-- | Finite natural numbers.
type Nat = Mu Maybe

-- | Represents partial functions that may eventually return a value (`Left`).
-- NB: This is a newtype so we can create the usual instances.
newtype Partial a = Partial {fromPartial :: Nu (Either a)}

-- TODO: There may be some way to do this over an arbitrary @newtype@, or at
--       least a way to do it over an arbitrary `Iso`.
insidePartial :: (Nu (Either a) -> Nu (Either b)) -> Partial a -> Partial b
insidePartial f = Partial . f . fromPartial

instance Functor Partial where
  fmap f = insidePartial (comap f)

instance Applicative Partial where
  pure = Partial . embed . Left
  ff <*> fa =
    flip insidePartial ff $
      elgotAna
        (seqEither project)
        ((fromPartial . flip fmap fa +++ Right) . project)

instance Monad Partial where
  pa >>= f = join' (fmap f pa)
    where
      join' =
        insidePartial $
          elgotAna (seqEither project) ((fromPartial +++ Right) . project)

-- | Always-infinite streams (as opposed to `Colist`, which _may_ terminate).
type Stream a = Nu ((,) a)

-- | A more general implementation of `fmap`, because it can also work to, from,
--   or within monomorphic structures, obviating the need for classes like
--  `Data.MonoTraversable.MonoFunctor`.
map :: (Recursive (->) t (f a), Steppable (->) u (f b), Bifunctor f) => (a -> b) -> t -> u
map f = cata (embed . first f)

-- | A version of `Yaya.Zoo.map` that applies to Corecursive structures.
comap ::
  (Projectable (->) t (f a), Corecursive (->) u (f b), Bifunctor f) =>
  (a -> b) ->
  t ->
  u
comap f = ana (first f . project)

-- TODO: Weaken the `Monad` constraint to `Applicative`.

-- | A more general implementation of `Data.Traversable.traverse`, because it
--   can also work to, from, or within monomorphic structures, obviating the
--   need for classes like `Data.MonoTraversable.MonoTraversable`.
traverse ::
  ( Recursive (->) t (f a),
    Steppable (->) u (f b),
    Bitraversable f,
    Traversable (f a),
    Monad m
  ) =>
  (a -> m b) ->
  t ->
  m u
traverse f = cata (fmap embed . bitraverse f pure <=< sequenceA)

-- | A more general implementation of `Data.Functor.contramap`, because it can
--   also work to, from, or within monomorphic structures.
contramap ::
  (Recursive (->) t (f b), Steppable (->) u (f a), Profunctor f) =>
  (a -> b) ->
  t ->
  u
contramap f = cata (embed . lmap f)

cocontramap ::
  (Projectable (->) t (f b), Corecursive (->) u (f a), Profunctor f) =>
  (a -> b) ->
  t ->
  u
cocontramap f = ana (lmap f . project)
