module Yaya.Unsafe.Zoo where

import           Control.Arrow
import           Control.Comonad.Cofree
import           Control.Comonad.Env
import           Control.Monad.Trans.Free
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Bitraversable

import           Yaya.Fold
import           Yaya.Fold.Native
import           Yaya.Pattern
import qualified Yaya.Unsafe.Fold as Unsafe
import qualified Yaya.Unsafe.Fold.Instances as Unsafe -- NB: extremely unsafe

chrono
  :: Functor f
  => GAlgebra (->) (Cofree f) f b
  -> GCoalgebra (->) (Free f) f a
  -> a
  -> b
chrono = Unsafe.ghylo (distCofreeT id) (Unsafe.seqFreeT id)

codyna :: Functor f => Algebra (->) f b -> GCoalgebra (->) (Free f) f a -> a -> b
codyna φ = Unsafe.ghylo distIdentity (Unsafe.seqFreeT id) (φ . fmap runIdentity)

-- | [Recursion Schemes for Dynamic Programming](https://www.researchgate.net/publication/221440162_Recursion_Schemes_for_Dynamic_Programming)
dyna :: Functor f => GAlgebra (->) (Cofree f) f b -> Coalgebra (->) f a -> a -> b
dyna φ ψ = Unsafe.ghylo (distCofreeT id) seqIdentity φ (fmap Identity . ψ)

-- | Unlike most `Unsafe.hylo`s, `elgot` composes an algebra and coalgebra in a
--   way that allows information to move between them. The coalgebra can return,
--   effectively, a pre-folded branch, short-circuiting parts of the process.
elgot :: Functor f => Algebra (->) f b -> ElgotCoalgebra (->) (Either b) f a -> a -> b
elgot φ ψ = Unsafe.hylo ((id ||| φ) . getCompose) (Compose . ψ)

-- | The dual of `elgot`, `coelgot` allows the /algebra/ to short-circuit in
--   some cases – operating directly on a part of the seed.
coelgot :: Functor f => ElgotAlgebra (->) ((,) a) f b -> Coalgebra (->) f a -> a -> b
coelgot φ ψ = Unsafe.hylo (φ . getCompose) (Compose . (id &&& ψ))

futu :: (Corecursive (->) t f, Functor f) => GCoalgebra (->) (Free f) f a -> a -> t
futu = gana (Unsafe.seqFreeT id)

gprepro
  :: (Steppable (->) t f, Recursive (->) t f, Functor f, Comonad w)
  => DistributiveLaw (->) f w
  -> GAlgebra (->) w f a
  -> (forall x. f x -> f x)
  -> t
  -> a
gprepro k φ e =
  Unsafe.ghylo k seqIdentity φ (fmap (Identity . cata (embed . e)) . project)

gpostpro
  :: (Steppable (->) t f, Corecursive (->) t f, Functor f, Monad m)
  => DistributiveLaw (->) m f
  -> (forall x. f x -> f x)
  -> GCoalgebra (->) m f a
  -> a
  -> t
gpostpro k e =
  Unsafe.ghylo distIdentity k (embed . fmap (ana (e . project) . runIdentity))

-- | The metamorphism definition from Gibbons’ paper.
stream :: Coalgebra (->) (XNor c) b -> (b -> a -> b) -> b -> [a] -> [c]
stream f g = fstream f g (const Neither)

-- | Basically the definition from Gibbons’ paper, except the flusher (@h@) is a
--  `Coalgebra` instead of an `unfold`.
fstream
  :: Coalgebra (->) (XNor c) b
  -> (b -> a -> b)
  -> Coalgebra (->) (XNor c) b
  -> b
  -> [a]
  -> [c]
fstream f g h =
  Unsafe.streamGApo
  h
  (\b -> case f b of
           Neither -> Nothing
           other   -> Just other)
  (\case
      Neither   -> Nothing
      Both a x' -> Just (flip g a, x'))

-- snoc :: [a] -> a -> [a]
-- snoc x a = x ++ [a]

-- x :: [Int]
-- x = stream project snoc [] [1, 2, 3, 4, 5]

-- TODO: Weaken `Monad` constraint to `Applicative`.
cotraverse
  :: ( Steppable (->) t (f a)
     , Steppable (->) u (f b)
     , Corecursive (->) u (f b)
     , Bitraversable f
     , Traversable (f b)
     , Monad m)
  => (a -> m b)
  -> t
  -> m u
cotraverse f = Unsafe.anaM (bitraverse f pure . project)

-- | Zygohistomorphic prepromorphism – everyone’s favorite recursion scheme joke.
zygoHistoPrepro
  :: (Steppable (->) t f, Recursive (->) t f, Functor f)
  => (f b -> b)
  -> (f (EnvT b (Cofree f) a) -> a)
  -> (forall c. f c -> f c)
  -> t
  -> a
zygoHistoPrepro φ' = gprepro (distEnvT φ' (distCofreeT id))
