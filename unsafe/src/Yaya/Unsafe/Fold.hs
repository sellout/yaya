{-# LANGUAGE Safe #-}

-- | Definitions and instances that use direct recursion, which (because of
--   laziness) can lead to non-termination.
module Yaya.Unsafe.Fold
  ( anaM,
    corecursivePrism,
    ganaM,
    ghylo,
    ghyloM,
    hylo,
    hyloM,
    stream',
    streamAna,
    streamGApo,
    unsafeAna,
    unsafeCata,
  )
where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category ((.)))
import "base" Control.Monad (Monad, (<=<))
import "base" Data.Function (flip, ($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Compose (Compose (Compose, getCompose))
import "base" Data.Traversable (Traversable (sequenceA))
import "comonad" Control.Comonad (Comonad (extract))
import "lens" Control.Lens (Prism', matching, prism, review)
import "yaya" Yaya.Fold
  ( Algebra,
    AlgebraM,
    Coalgebra,
    CoalgebraM,
    CoalgebraPrism,
    Corecursive (ana),
    DistributiveLaw,
    GAlgebra,
    GAlgebraM,
    GCoalgebra,
    GCoalgebraM,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    lowerAlgebra,
    lowerAlgebraM,
    lowerCoalgebra,
    lowerCoalgebraM,
  )
import "yaya" Yaya.Pattern (Maybe, Pair, maybe, uncurry)

-- | Instances leak transitively, so while "Yaya.Unsafe.Fold.Instances" exists,
--   it should only be used when it is unavoidable. If you are explicitly
--   folding a structure unsafely, use this function instead of importing that
--   module.
unsafeAna :: (Steppable (->) t f, Functor f) => Coalgebra (->) f a -> a -> t
unsafeAna = hylo embed

-- | Instances leak transitively, so while "Yaya.Unsafe.Fold.Instances" exists,
--   it should only be used when it is unavoidable. If you are explicitly
--   unfolding a structure unsafely, use this function instead of importing that
--   module.
--
--   Should one prefer `unsafeAna` or `unsafeCata` in cases where both are
--   applicable?
-- - one may provide weaker constraints than the other in certain cases (e.g.,
--   on its own, `unsafeCata` only requires `Projectable` on the source, but
--  `unsafeAna` requires `Steppable` on the target. Depending on what other
--   constraints already exist on the function, either one may ultimately be
--   less constrained.
-- - they may fail differently: `unsafeCata` (folding a potentially-infinite
--   structure) is likely to result in non-termination, whereas `unsafeAna`
--   (building a potentially-infinite structure strictly) is likely to use up
--   the memory or overflow the stack.
unsafeCata :: (Projectable (->) t f, Functor f) => Algebra (->) f a -> t -> a
unsafeCata = flip hylo project

-- | This can’t be implemented in a total fashion. There is a /similar/ approach
--   that can be total – with @ψ :: `CoalgebraM` (->) m f a@, @`ana` (`Compose`
--  . ψ)@ results in something like @`Nu` (`Compose` m f)@ which is akin to an
--   effectful stream.
anaM ::
  (Monad m, Steppable (->) t f, Traversable f) =>
  CoalgebraM (->) m f a ->
  a ->
  m t
anaM = hyloM (pure . embed)

ganaM ::
  (Monad m, Monad n, Traversable n, Steppable (->) t f, Traversable f) =>
  DistributiveLaw (->) n f ->
  GCoalgebraM (->) m n f a ->
  a ->
  m t
ganaM k ψ = anaM (lowerCoalgebraM k ψ) . pure

-- | Fusion of an 'ana' and a 'cata'.
hylo :: (Functor f) => Algebra (->) f b -> Coalgebra (->) f a -> a -> b
hylo φ ψ = go
  where
    go = φ . fmap go . ψ

ghylo ::
  (Comonad w, Monad m, Functor f) =>
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) m f ->
  GAlgebra (->) w f b ->
  GCoalgebra (->) m f a ->
  a ->
  b
ghylo w m φ ψ =
  extract . hylo (lowerAlgebra w φ) (lowerCoalgebra m ψ) . pure

hyloM ::
  (Monad m, Traversable f) =>
  AlgebraM (->) m f b ->
  CoalgebraM (->) m f a ->
  a ->
  m b
hyloM φ ψ = hylo (φ <=< sequenceA <=< getCompose) (Compose . ψ)

ghyloM ::
  (Comonad w, Traversable w, Monad m, Traversable f, Monad n, Traversable n) =>
  DistributiveLaw (->) f w ->
  DistributiveLaw (->) n f ->
  GAlgebraM (->) m w f b ->
  GCoalgebraM (->) m n f a ->
  a ->
  m b
ghyloM w n φ ψ =
  fmap extract . hyloM (lowerAlgebraM w φ) (lowerCoalgebraM n ψ) . pure

-- | This is the core operation for all metamorphisms. It generally shouldn’t be
--   used directly, but is exposed in case you come up with a novel accumulation
--   function to use.
--
--   Metamorphisms are conceptually a fold followed by an unfold (effectively
--   the reverse of a hylomorphism). Many are equivalent to @`ana` ψ `.` `cata`
--   φ@, but some can be processed incrementally, forming a family of
--   “[streaming
--   metamorphisms](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/metamorphisms-scp.pdf)”,
--   which are the ones captured here.
--
--  __FIXME__: What happens when this is given a branching structure? Where does
--             that cause a problem?
--
--  __NB__: See https://gist.github.com/sellout/4709e723cb649110af00217486c4466b
--          for some commentary and explanation.
stream' ::
  ( Projectable (->) input inputf,
    Steppable (->) output outputf,
    Functor outputf
  ) =>
  -- | Lazily processes the state into additional output elements. This should
  --   return `Nothing` when the state doesn’t allow any more output to be
  --   generated, causing control to be transferred back to the accumulator
  --   algebra.
  --
  -- > state -> Maybe (outputf state)
  CoalgebraM (->) Maybe outputf state ->
  -- | The general state accumulation function, this is specialized in the other
  --   @stream*@ functions. Given a state and a continuation function, converts
  --   the entire input to output.
  --
  --  __TODO__: Consider whether it’d be useful/possible to use
  --
  --          > forall x. state -> ((state -> state) -> x -> output) -> inputf x -> output
  --
  --            to prevent the function from consuming more than one element of
  --            the input per call.
  (state -> ((state -> state) -> input -> output) -> inputf input -> output) ->
  -- | The initial state.
  state ->
  -- | The `Recursive` (well, `Projectable`) input.
  input ->
  -- | The `Corecursive` (well, `Steppable`) output.
  output
stream' process accum = go
  where
    go state input =
      maybe
        (accum state (go . ($ state)) (project input))
        (embed . fmap (`go` input))
        $ process state

-- | Gibbons’ metamorphism. It lazily folds a (necessarily infinite) value,
--   incrementally re-expanding that value into some new representation. See
--  `stream'` for more on metamorphisms.
--
--  __FIXME__: What happens when this is given a finite structure?
--
--   The “Ana” in the name parallels the naming of `streamGApo`, where this form
--   lacks the helper algebra, in the same way that `ana` lacks the helper
--   algebra that `Yaya.Zoo.gapo` has.
streamAna ::
  ( Projectable (->) input inputf,
    Steppable (->) output outputf,
    Functor outputf
  ) =>
  -- | Lazily processes the state into additional output elements. This should
  --   return `Nothing` when the state doesn’t allow any more output to be
  --   generated, causing control to be transferred back to the accumulator
  --   algebra.
  --
  -- > state -> Maybe (outputf state)
  CoalgebraM (->) Maybe outputf state ->
  -- | Accumulates more elements from the input into the state. It returns a
  --   function to modify the previous state as well as the remaining input.
  --   This passes control back to the processing coalgebra after each call,
  --   allowing as much output to be generated from as little input as possible.
  --
  -- > inputf input -> (state -> state, input)
  AlgebraM (->) (Pair (state -> state)) inputf input ->
  -- | The initial state.
  state ->
  -- | The `Recursive` (well, `Projectable`) input.
  input ->
  -- | The `Corecursive` (well, `Steppable`) output.
  output
streamAna process accum = stream' process $ \_state cont -> uncurry cont . accum

-- | Another form of Gibbons’ metamorphism. This one can be applied to non-
--   infinite inputs and takes an additional “flushing” coalgebra to be applied
--   after all the input has been consumed. See `stream'` for more on
--  metamorphisms.
--
--   The “GApo” in the name comes from the parallel with `Yaya.Zoo.gapo`, where
--   a “helper” `Coalgebra` (the “flusher” in this case) can be applied when the
--   primary algebra “fails”. This is also why the arguments are re-ordered
--   relative to Gibbons’ `Yaya.Unsafe.Zoo.fstream` – to make the parallel with
--   @gapo@ more obvious.
streamGApo ::
  ( Projectable (->) input inputf,
    Steppable (->) output outputf,
    Corecursive (->) output outputf,
    Functor outputf
  ) =>
  -- | The flushing coalgebra that consumes the remaining state after the input
  --   has been fully consumed.
  Coalgebra (->) outputf state ->
  -- | Lazily processes the state into additional output elements. This should
  --   return `Nothing` when the state doesn’t allow any more output to be
  --   generated, causing control to be transferred back to the accumulator
  --   algebra.
  --
  -- > state -> Maybe (outputf state)
  CoalgebraM (->) Maybe outputf state ->
  -- | Accumulates more elements from the input into the state. It returns a
  --   function to modify the previous state as well as the remaining input.
  --   This passes control back to the processing coalgebra after each call,
  --   allowing as much output to be generated from as little input as possible.
  --   This should return `Nothing` when the input is consumed, causing control
  --   to be transferred to the flushing coalgebra instead of the processing
  --   coalgebra.
  (inputf input -> Maybe (Pair (state -> state) input)) ->
  -- | The initial state.
  state ->
  -- | The `Recursive` (well, `Projectable`) input.
  input ->
  -- | The `Corecursive` output.
  output
streamGApo flush process accum =
  stream' process $
    \state cont -> maybe (ana flush state) (uncurry cont) . accum

corecursivePrism ::
  (Steppable (->) t f, Recursive (->) t f, Traversable f) =>
  CoalgebraPrism f a ->
  Prism' a t
corecursivePrism alg = prism (cata $ review alg) (anaM $ matching alg)
