{-# LANGUAGE Safe #-}

module Yaya.Native.Zoo
  ( histo,
  )
where

import "base" Data.Functor (Functor)
import "yaya" Yaya.Fold (GAlgebra, Recursive, gcata)
import "this" Yaya.Native.Fold (distCofreeT)

histo ::
  (Recursive (->) t f, Functor f) => GAlgebra (->) (Cofree f) f a -> t -> a
histo = gcata $ distCofreeT id
