{-# LANGUAGE Safe #-}

module Yaya.Native.Unsafe.Zoo
  ( chrono,
    dyna,
    zygoHistoPrepro,
  )
where

import "base" Control.Category (id, (.))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Identity (Identity (Identity))
import "comonad" Control.Comonad.Env (EnvT)
import "free" Control.Comonad.Cofree (Cofree)
import "free" Control.Monad.Trans.Free (Free)
import "yaya" Yaya.Fold
  ( Coalgebra,
    GAlgebra,
    GCoalgebra,
    Recursive,
    Steppable,
    distEnvT,
    seqIdentity,
  )
import qualified "yaya-unsafe" Yaya.Unsafe.Fold as Unsafe
import qualified "yaya-unsafe" Yaya.Unsafe.Zoo as Unsafe
import "this" Yaya.Native.Fold (distCofreeT)

chrono ::
  (Functor f) =>
  GAlgebra (->) (Cofree f) f b ->
  GCoalgebra (->) (Free f) f a ->
  a ->
  b
chrono = Unsafe.ghylo (distCofreeT id) (Unsafe.seqFreeT id)

-- | [Recursion Schemes for Dynamic Programming](https://www.researchgate.net/publication/221440162_Recursion_Schemes_for_Dynamic_Programming)
dyna :: (Functor f) => GAlgebra (->) (Cofree f) f b -> Coalgebra (->) f a -> a -> b
dyna φ ψ = Unsafe.ghylo (distCofreeT id) seqIdentity φ (fmap Identity . ψ)

-- | Zygohistomorphic prepromorphism – everyone’s favorite recursion scheme joke.
zygoHistoPrepro ::
  (Steppable (->) t f, Recursive (->) t f, Functor f) =>
  (f b -> b) ->
  (f (EnvT b (Cofree f) a) -> a) ->
  (forall c. f c -> f c) ->
  t ->
  a
zygoHistoPrepro φ' = Unsafe.gprepro (distEnvT φ' (distCofreeT id))
