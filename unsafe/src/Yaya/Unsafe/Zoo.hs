{-# LANGUAGE Safe #-}

module Yaya.Unsafe.Zoo
  ( codyna,
    coelgot,
    cotraverse,
    elgot,
    fstream,
    futu,
    gpostpro,
    gprepro,
    stream,
  )
where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category (id, (.)))
import "base" Control.Monad (Monad)
import "base" Data.Bifunctor (Bifunctor (second))
import "base" Data.Bitraversable (Bitraversable (bitraverse))
import "base" Data.Function (const, flip)
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Compose (Compose (Compose, getCompose))
import "base" Data.Functor.Identity (Identity (Identity, runIdentity))
import "base" Data.Traversable (Traversable)
import "comonad" Control.Comonad (Comonad)
import "free" Control.Monad.Trans.Free (Free)
import "yaya" Yaya.Fold
  ( Algebra,
    Coalgebra,
    Corecursive (ana),
    DistributiveLaw,
    ElgotAlgebra,
    ElgotCoalgebra,
    GAlgebra,
    GCoalgebra,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    distIdentity,
    gana,
    seqIdentity,
  )
import "yaya" Yaya.Fold.Common (diagonal, fromEither)
import "yaya" Yaya.Pattern (Either, Maybe (Nothing), Pair ((:!:)), XNor (Both, Neither))
import qualified "this" Yaya.Unsafe.Fold as Unsafe

codyna :: (Functor f) => Algebra (->) f b -> GCoalgebra (->) (Free f) f a -> a -> b
codyna φ = Unsafe.ghylo distIdentity (Unsafe.seqFreeT id) (φ . fmap runIdentity)

-- | Unlike most `Unsafe.hylo`s, `elgot` composes an algebra and coalgebra in a
--   way that allows information to move between them. The coalgebra can return,
--   effectively, a pre-folded branch, short-circuiting parts of the process.
elgot :: (Functor f) => Algebra (->) f b -> ElgotCoalgebra (->) (Either b) f a -> a -> b
elgot φ ψ = Unsafe.hylo (fromEither . second φ . getCompose) (Compose . ψ)

-- | The dual of `elgot`, `coelgot` allows the /algebra/ to short-circuit in
--   some cases – operating directly on a part of the seed.
coelgot :: (Functor f) => ElgotAlgebra (->) (Pair a) f b -> Coalgebra (->) f a -> a -> b
coelgot φ ψ = Unsafe.hylo (φ . getCompose) (Compose . second ψ . diagonal)

futu :: (Corecursive (->) t f, Functor f) => GCoalgebra (->) (Free f) f a -> a -> t
futu = gana (Unsafe.seqFreeT id)

gprepro ::
  (Steppable (->) t f, Recursive (->) t f, Functor f, Comonad w) =>
  DistributiveLaw (->) f w ->
  GAlgebra (->) w f a ->
  (forall x. f x -> f x) ->
  t ->
  a
gprepro k φ e =
  Unsafe.ghylo k seqIdentity φ (fmap (Identity . cata (embed . e)) . project)

gpostpro ::
  (Steppable (->) t f, Corecursive (->) t f, Functor f, Monad m) =>
  DistributiveLaw (->) m f ->
  (forall x. f x -> f x) ->
  GCoalgebra (->) m f a ->
  a ->
  t
gpostpro k e =
  Unsafe.ghylo distIdentity k (embed . fmap (ana (e . project) . runIdentity))

-- | The metamorphism definition from [Gibbons’
--   paper](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/metamorphisms-scp.pdf).
stream ::
  ( Projectable (->) l (XNor a),
    Steppable (->) s (XNor c),
    Corecursive (->) s (XNor c)
  ) =>
  Coalgebra (->) (XNor c) b ->
  (b -> a -> b) ->
  b ->
  l ->
  s
stream f g = fstream f g (const Neither)

-- | Basically the definition from [Gibbons’
--   paper](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/metamorphisms-scp.pdf),
--   except the flusher is a `Coalgebra` instead of an unfold.
--
--   The implementation shows how `Unsafe.streamGApo` generalizes Gibbons’
--  `fstream` (and `Unsafe.stream'` even more so).
fstream ::
  ( Projectable (->) l (XNor a),
    Steppable (->) s (XNor c),
    Corecursive (->) s (XNor c)
  ) =>
  Coalgebra (->) (XNor c) b ->
  (b -> a -> b) ->
  -- | The flusher.
  Coalgebra (->) (XNor c) b ->
  b ->
  l ->
  s
fstream f g h =
  Unsafe.streamGApo
    h
    ( \b -> case f b of
        Neither -> Nothing
        other -> pure other
    )
    ( \case
        Neither -> Nothing
        Both a x' -> pure (flip g a :!: x')
    )

-- snoc :: [a] -> a -> [a]
-- snoc x a = x ++ [a]

-- x :: [Int]
-- x = stream project snoc [] [1, 2, 3, 4, 5]

-- TODO: Weaken `Monad` constraint to `Applicative`.
cotraverse ::
  ( Steppable (->) t (f a),
    Steppable (->) u (f b),
    Bitraversable f,
    Traversable (f b),
    Monad m
  ) =>
  (a -> m b) ->
  t ->
  m u
cotraverse f = Unsafe.anaM (bitraverse f pure . project)
