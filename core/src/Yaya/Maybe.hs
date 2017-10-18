-- | Operations on natural-number-like structures.
module Yaya.Maybe where

import Yaya
import Yaya.Control

zeroN :: (Cursive t Maybe) => t
zeroN = embed Nothing

succN :: (Cursive t Maybe) => t -> t
succN = embed . Just

height :: (Foldable f, Cursive n Maybe, Ord n) => Algebra f n
height = foldr (max . succN) zeroN
