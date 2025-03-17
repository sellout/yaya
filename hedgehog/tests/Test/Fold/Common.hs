{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold.Common (tests) where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Ord ((<))
import safe "base" Data.Word (Word8)
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
import safe "yaya" Yaya.Fold (Mu, Recursive, cata, project, zipAlgebras)
import safe "yaya" Yaya.Fold.Common (height, size)
import safe "yaya" Yaya.Pattern (Maybe, uncurry)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (Expr, exprInRange)
import safe "base" Prelude (toInteger)

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
--
--  __FIXME__: Remove this instance, since we already have one downstream.
instance Recursive (->) Word8 Maybe where
  cata φ = φ . fmap (cata φ) . project

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    . ( assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
          <=< forAll
      )
    $ exprInRange @(Mu Expr) Gen.word8 0 20

tests :: IO Bool
tests = checkParallel $$discover
