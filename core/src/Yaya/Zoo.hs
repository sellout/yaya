{-# LANGUAGE Safe #-}

-- | Contains all the commonly-named folds that aren’t core to the library. In
--   general, this can be seen as a mapping from names you may have heard or
--   read in a paper to how Yaya expects you to achieve the same end. Of course,
--   you can always import this module and use the “common” name as well.
module Yaya.Zoo
  ( Colist,
    List,
    Nat,
    NonEmptyList,
    Partial (Partial),
    Stream,
    apo,
    cataM,
    cocontramap,
    comap,
    comutu,
    contramap,
    fromPartial,
    gapo,
    gmutu,
    histo,
    insidePartial,
    map,
    mutu,
    mutuM,
    para,
    traverse,
    zygo,
    zygoM,
  )
where

import "base" Control.Applicative (Applicative, pure, (<*>))
import "base" Control.Category (id, (.))
import "base" Control.Monad (Monad, (<=<), (>>=))
import "base" Data.Bifunctor (Bifunctor, bimap, first)
import "base" Data.Bitraversable (Bitraversable, bisequence, bitraverse)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Traversable (Traversable, sequenceA)
import "comonad" Control.Comonad (Comonad, duplicate, extract)
import "comonad" Control.Comonad.Env (EnvT (EnvT))
import "free" Control.Comonad.Cofree (Cofree)
import "profunctors" Data.Profunctor (Profunctor, lmap)
import "this" Yaya.Fold
  ( Algebra,
    AlgebraM,
    Coalgebra,
    Corecursive,
    DistributiveLaw,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    Mu,
    Nu,
    Projectable,
    Recursive,
    Steppable,
    ana,
    cata,
    distTuple,
    elgotAna,
    embed,
    gana,
    gcata,
    project,
    seqEither,
  )
import "this" Yaya.Fold.Common (diagonal, fromEither)
import "this" Yaya.Fold.Native (distCofreeT)
import "this" Yaya.Pattern
  ( AndMaybe,
    Either (Left, Right),
    Maybe,
    Pair ((:!:)),
    XNor,
    fst,
    snd,
    swap,
    uncurry,
  )

-- | A generalized form of `apo`, where, rather than returning a complete
--   branch, you can return a value of another type, provided there is a
--   corresponding `Coalgebra` to expand the value into the same fixed-point
--   result type.
gapo ::
  (Corecursive (->) t f, Functor f) =>
  Coalgebra (->) f b ->
  GCoalgebra (->) (Either b) f a ->
  a ->
  t
gapo ψ = gana $ seqEither ψ

-- | A recursion scheme that allows you to return a complete branch when
--   unfolding.
apo ::
  (Projectable (->) t f, Corecursive (->) t f, Functor f) =>
  GCoalgebra (->) (Either t) f a ->
  a ->
  t
apo = gapo project

-- | If you have a monadic algebra, you can fold it by distributing the monad
--   over the algebra.
cataM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f a ->
  t ->
  m a
cataM = cata . (<=< sequenceA)

-- | A recursion scheme that allows two algebras to see each others’ results. (A
--   generalization of `zygo`.) This is an example that falls outside the scope
--   of “comonadic folds”, but _would_ be covered by “adjoint folds”.
mutu ::
  (Recursive (->) t f, Functor f) =>
  GAlgebra (->) (Pair a) f b ->
  GAlgebra (->) (Pair b) f a ->
  t ->
  a
mutu φ' φ = extract . cata (bimap (φ' . fmap swap) φ . diagonal)

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
        . fmap (fmap (uncurry EnvT) . distProd . bimap extract duplicate)
    distProd p =
      let a = fst p
       in fmap (a :!:) (snd p)

-- | As the name implies, this is the dual of `mutu`, and thus generalizes
--  `gapo`. Each coalgebra can return a value of an alternative type, which
--   causes expansion to continue with the other coalgebra.
comutu ::
  (Corecursive (->) t f, Functor f) =>
  GCoalgebra (->) (Either a) f b ->
  GCoalgebra (->) (Either b) f a ->
  a ->
  t
comutu ψ' ψ = ana (fromEither . bimap (fmap swapEither . ψ') ψ) . pure
  where
    swapEither = \case
      Left x -> Right x
      Right y -> Left y

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
  GAlgebraM (->) m (Pair a) f b ->
  GAlgebraM (->) m (Pair b) f a ->
  t ->
  m a
mutuM φ' φ = fmap snd . cataM (bisequence . bimap (φ' . fmap swap) φ . diagonal)

histo ::
  (Recursive (->) t f, Functor f) => GAlgebra (->) (Cofree f) f a -> t -> a
histo = gcata $ distCofreeT id

-- | A recursion scheme that gives you access to the original structure as you
--   fold. (A specialization of `zygo`.)
para ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) =>
  GAlgebra (->) (Pair t) f a ->
  t ->
  a
para = zygo embed

-- | A recursion scheme that uses a “helper algebra” to provide additional
--   information when folding. (A generalization of `para`, and specialization
--   of `mutu`.)
zygo ::
  (Recursive (->) t f, Functor f) =>
  Algebra (->) f b ->
  GAlgebra (->) (Pair b) f a ->
  t ->
  a
zygo φ = gcata $ distTuple φ

-- | This definition is different from the one given by @`gcataM` `.`
--  `distTuple`@ because it has a monadic “helper” algebra. But at least it
--   gives us the opportunity to show how `zygo` is a specialization of `mutu`.
zygoM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f b ->
  GAlgebraM (->) m (Pair b) f a ->
  t ->
  m a
zygoM = mutuM . (. fmap snd)

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
        (bimap (fromPartial . flip fmap fa) Right . project)

instance Monad Partial where
  pa >>= f = join' (fmap f pa)
    where
      join' =
        insidePartial $
          elgotAna (seqEither project) (bimap fromPartial Right . project)

-- | Always-infinite streams (as opposed to `Colist`, which _may_ terminate).
type Stream a = Nu (Pair a)

-- | A more general implementation of `fmap`, because it can also work to, from,
--   or within monomorphic structures, obviating the need for classes like
--  `Data.MonoTraversable.MonoFunctor`.
map ::
  (Recursive (->) t (f a), Steppable (->) u (f b), Bifunctor f) =>
  (a -> b) ->
  t ->
  u
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
