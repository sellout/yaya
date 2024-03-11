{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Fold.Common (tests) where

import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Functor (Functor (fmap))
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
import safe "yaya" Yaya.Fold (Recursive (cata), zipAlgebras)
import safe "yaya" Yaya.Fold.Common (height, size)
import safe "yaya" Yaya.Pattern (uncurry)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (genMuExpr)
import safe "base" Prelude (Integral (toInteger))

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    ( assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
        =<< forAll (Gen.sized genMuExpr)
    )

tests :: IO Bool
tests = checkParallel $$discover
