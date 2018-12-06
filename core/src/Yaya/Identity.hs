-- | Operations on the `Identity` functor.
module Yaya.Identity where

import Data.Functor.Identity

import Yaya
import Yaya.Control
import Yaya.Data

type Void = Mu Identity

never :: Coalgebra Identity a
never = Identity

absurd :: Recursive t Identity => t -> a
absurd = cata runIdentity

vacuous :: (Functor f, Recursive t Identity) => f t -> f a
vacuous = fmap absurd
