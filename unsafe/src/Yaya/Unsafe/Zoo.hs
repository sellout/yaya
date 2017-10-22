module Yaya.Unsafe.Zoo where

import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Monad.Trans.Free
import Data.Functor.Identity
import Data.Bitraversable

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Unsafe.Control
import Yaya.Unsafe.Data

chrono :: Functor f => GAlgebra (Cofree f) f b -> GCoalgebra (Free f) f a -> a -> b
chrono = ghylo (distGHisto id) (distGFutu id)

codyna :: Functor f => Algebra f b -> GCoalgebra (Free f) f a -> a -> b
codyna φ = ghylo distCata (distGFutu id) $ φ . fmap runIdentity

-- | [Recursion Schemes for Dynamic Programming](https://www.researchgate.net/publication/221440162_Recursion_Schemes_for_Dynamic_Programming)
dyna :: Functor f => GAlgebra (Cofree f) f b -> Coalgebra f a -> a -> b
dyna φ ψ = ghylo (distGHisto id) distAna φ $ fmap Identity . ψ

futu :: (Corecursive t f, Functor f) => GCoalgebra (Free f) f a -> a -> t
futu = gana $ distGFutu id

histo :: (Recursive t f, Functor f) => GAlgebra (Cofree f) f a -> t -> a
histo = gcata $ distGHisto id

-- | The metamorphism definition from Gibbons’ paper.
stream :: Coalgebra (XNor c) b -> (b -> a -> b) -> b -> [a] -> [c]
stream f g = fstream f g (const None)

-- | Basically the definition from Gibbons’ paper, except the flusher (`h`) is a
--  'Coalgebra' instead of an 'unfold'.
fstream
  :: Coalgebra (XNor c) b
  -> (b -> a -> b)
  -> Coalgebra (XNor c) b
  -> b
  -> [a]
  -> [c]
fstream f g h = streamGApo h
                           (\b -> case f b of
                                    None -> Nothing
                                    other -> Just other)
                           (\case
                               None -> Nothing
                               Both a x' -> Just (flip g a, x'))

-- snoc :: [a] -> a -> [a]
-- snoc x a = x ++ [a]

-- x :: [Int]
-- x = stream project snoc [] [1, 2, 3, 4, 5]

-- TODO: Weaken 'Monad' constraint to 'Applicative'.
cotraverse
  :: ( Cursive t (f a)
     , Cursive u (f b)
     , Corecursive u (f b)
     , Bitraversable f
     , Traversable (f b)
     , Monad m)
  => (a -> m b)
  -> t
  -> m u
cotraverse f = anaM $ bitraverse f pure . project

-- | Zygohistomorphic prepromorphism – everyone’s favorite recursion scheme joke.
zygoHistoPrepro
  :: (Cursive t f, Recursive t f, Functor f)
  => (f b -> b)
  -> (forall c. f c -> f c)
  -> (f (EnvT b (Cofree f) a) -> a)
  -> t
  -> a
zygoHistoPrepro φ' e φ = gprepro (distZygoT φ' $ distGHisto id) e φ
