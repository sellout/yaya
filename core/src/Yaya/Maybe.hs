-- | Operations on natural-number-like structures.
module Yaya.Maybe where

import Yaya
import Yaya.Control

zeroN :: Steppable t Maybe => t
zeroN = embed Nothing

succN :: Steppable t Maybe => t -> t
succN = embed . Just

height :: (Foldable f, Steppable n Maybe, Ord n) => Algebra f n
height = foldr (max . succN) zeroN
