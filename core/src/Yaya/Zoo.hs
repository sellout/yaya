{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2017 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   Contains all the commonly-named folds that aren’t core to the library. In
--   general, this can be seen as a mapping from names you may have heard or
--   read in a paper to how Yaya expects you to achieve the same end. Of course,
--   you can always import this module and use the “common” name as well.
--
-- @since 0.1.0
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
    futu,
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
import "free" Control.Monad.Trans.Free (FreeF)
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
    distCofreeT,
    distTuple,
    elgotAna,
    embed,
    gana,
    gcata,
    project,
    seqEither,
    seqFreeT,
  )
import "this" Yaya.Fold.Common (diagonal, fromEither)
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
--
-- @since 0.6.2
gapo ::
  (Corecursive (->) t f, Functor f) =>
  Coalgebra (->) f b ->
  GCoalgebra (->) (Either b) f a ->
  a ->
  t
gapo ψ = gana $ seqEither ψ

-- | A recursion scheme that allows you to return a complete branch when
--   unfolding.
--
-- @since 0.1.0
apo ::
  (Projectable (->) t f, Corecursive (->) t f, Functor f) =>
  GCoalgebra (->) (Either t) f a ->
  a ->
  t
apo = gapo project

-- | If you have a monadic algebra, you can fold it by distributing the monad
--   over the algebra.
--
-- @since 0.1.0
cataM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f a ->
  t ->
  m a
cataM = cata . (<=< sequenceA)

-- | A recursion scheme that allows two algebras to see each others’ results. (A
--   generalization of `zygo`.) This is an example that falls outside the scope
--   of “comonadic folds”, but /would/ be covered by “adjoint folds”.
--
-- @since 0.1.0
mutu ::
  (Recursive (->) t f, Functor f) =>
  GAlgebra (->) (Pair a) f b ->
  GAlgebra (->) (Pair b) f a ->
  t ->
  a
mutu φ' φ = extract . cata (bimap (φ' . fmap swap) φ . diagonal)

-- | A generalization of `mutu` in which each of the two algebras is a
--   `GAlgebra` over its own comonad (with its own `DistributiveLaw`), and sees
--   the other’s result via `EnvT`.
--
-- @since 0.1.0
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
--
-- @since 0.1.0
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

-- | A monadic version of `mutu`, sequencing the effects of both algebras.
--
-- @since 0.1.0
mutuM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  GAlgebraM (->) m (Pair a) f b ->
  GAlgebraM (->) m (Pair b) f a ->
  t ->
  m a
mutuM φ' φ = fmap snd . cataM (bisequence . bimap (φ' . fmap swap) φ . diagonal)

-- | A futumorphism – an anamorphism whose coalgebra can produce several layers
--   at once, as a free monad over @f@ (any @freef@ that behaves like
--   `Control.Monad.Free.Free`). The dual of `histo`.
--
-- @since 1.0.0
futu ::
  ( Corecursive (->) t f,
    Functor f,
    forall x. Recursive (->) (freef x) (FreeF f x),
    forall x. Steppable (->) (freef x) (FreeF f x),
    Monad freef
  ) =>
  GCoalgebra (->) freef f a ->
  a ->
  t
futu = gana (seqFreeT id)

-- | A histomorphism – a catamorphism whose algebra sees the results for all
--   substructures, not only the immediate ones, recorded in a cofree comonad
--   over @f@ (any @cofreef@ that behaves like `Control.Comonad.Cofree.Cofree`).
--   The dual of `futu`.
--
-- @since 0.1.0
histo ::
  ( Recursive (->) t f,
    forall x. Corecursive (->) (cofreef x) (EnvT x f),
    forall x. Projectable (->) (cofreef x) (EnvT x f),
    Comonad cofreef,
    Functor f
  ) =>
  GAlgebra (->) cofreef f a ->
  t ->
  a
histo = gcata $ distCofreeT id

-- | A recursion scheme that gives you access to the original structure as you
--   fold. (A specialization of `zygo`.)
--
-- @since 0.1.0
para ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) =>
  GAlgebra (->) (Pair t) f a ->
  t ->
  a
para = zygo embed

-- | A recursion scheme that uses a “helper algebra” to provide additional
--   information when folding. (A generalization of `para`, and specialization
--   of `mutu`.)
--
-- @since 0.1.0
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
--
-- @since 0.1.0
zygoM ::
  (Monad m, Recursive (->) t f, Traversable f) =>
  AlgebraM (->) m f b ->
  GAlgebraM (->) m (Pair b) f a ->
  t ->
  m a
zygoM = mutuM . (. fmap snd)

-- | Potentially-infinite lists, like `[]`.
--
-- @since 0.1.0
type Colist a = Nu (XNor a)

-- | Finite lists.
--
-- @since 0.1.0
type List a = Mu (XNor a)

-- | Finite non-empty lists.
--
-- @since 0.1.0
type NonEmptyList a = Mu (AndMaybe a)

-- | Finite natural numbers.
--
-- @since 0.1.0
type Nat = Mu Maybe

-- | Represents partial functions that may eventually return a value (`Left`).
-- NB: This is a newtype so we can create the usual instances.
--
-- @since 0.1.0
newtype Partial a
  = -- | A computation that might not terminate.
    --
    -- @since 0.1.0
    Partial
    { -- | The underlying fixed-point, in which each `Right` layer is a step of
      --   delay and a `Left` holds the result.
      --
      -- @since 0.1.0
      fromPartial :: Nu (Either a)
    }

type role Partial representational

-- TODO: There may be some way to do this over an arbitrary @newtype@, or at
--       least a way to do it over an arbitrary `Iso`.

-- | Lifts a function on the underlying fixed-points to one on `Partial`s.
--
-- @since 0.1.0
insidePartial :: (Nu (Either a) -> Nu (Either b)) -> Partial a -> Partial b
insidePartial f = Partial . f . fromPartial

-- | @since 0.1.0
instance Functor Partial where
  fmap f = insidePartial (comap f)

-- | @since 0.1.0
instance Applicative Partial where
  pure = Partial . embed . Left
  ff <*> fa =
    flip insidePartial ff $
      elgotAna
        (seqEither project)
        (bimap (fromPartial . flip fmap fa) Right . project)

-- | @since 0.1.0
instance Monad Partial where
  pa >>= f = join' (fmap f pa)
    where
      join' =
        insidePartial $
          elgotAna (seqEither project) (bimap fromPartial Right . project)

-- | Always-infinite streams (as opposed to `Colist`, which /may/ terminate).
--
-- @since 0.1.0
type Stream a = Nu (Pair a)

-- | A more general implementation of `fmap`, because it can also work to, from,
--   or within monomorphic structures, obviating the need for classes like
--  `Data.MonoTraversable.MonoFunctor`.
--
-- @since 0.1.0
map ::
  (Recursive (->) t (f a), Steppable (->) u (f b), Bifunctor f) =>
  (a -> b) ->
  t ->
  u
map f = cata (embed . first f)

-- | A version of `Yaya.Zoo.map` that applies to Corecursive structures.
--
-- @since 0.1.0
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
--
-- @since 0.1.0
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
--
-- @since 0.1.0
contramap ::
  (Recursive (->) t (f b), Steppable (->) u (f a), Profunctor f) =>
  (a -> b) ->
  t ->
  u
contramap f = cata (embed . lmap f)

-- | A version of `contramap` that applies to `Corecursive` structures.
--
-- @since 0.1.0
cocontramap ::
  (Projectable (->) t (f b), Corecursive (->) u (f a), Profunctor f) =>
  (a -> b) ->
  t ->
  u
cocontramap f = ana (lmap f . project)
