{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold.Common (tests) where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Ord (Ord ((<)))
import safe "base" System.IO (IO)
import safe "hedgehog" Hedgehog
  ( Property,
    assert,
    checkParallel,
    discover,
    forAll,
    property,
  )
import safe qualified "hedgehog" Hedgehog.Gen as Gen
import safe "yaya" Yaya.Fold (Mu, cata, zipAlgebras)
import safe "yaya" Yaya.Fold.Common (height, size)
import safe "yaya" Yaya.Pattern (uncurry)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, exprInRange)
import safe "base" Prelude (Integral (toInteger))

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    . assert
    . uncurry (<)
    . fmap toInteger
    . cata (zipAlgebras height size)
    <=< forAll
    $ exprInRange @(Mu Expr) Gen.word8 0 20

tests :: IO Bool
tests = checkParallel $$discover
