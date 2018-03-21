-- | Operations on natural-number-like structures.
module Yaya.Maybe where

import Yaya
import Yaya.Control

zeroN :: Embeddable t Maybe => t
zeroN = embed Nothing

succN :: Embeddable t Maybe => t -> t
succN = embed . Just

height :: (Foldable f, Embeddable n Maybe, Ord n) => Algebra f n
height = foldr (max . succN) zeroN
