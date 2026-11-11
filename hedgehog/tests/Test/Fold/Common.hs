{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Copyright: 2018 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 0.2.1.3
module Test.Fold.Common (tests) where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Functor (fmap)
import safe "base" Data.Ord ((<))
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
import safe "yaya" Yaya.Fold (cata, zipAlgebras)
import safe "yaya" Yaya.Fold.Common (height, size)
import safe "yaya" Yaya.Pattern (uncurry)
import safe "yaya-hedgehog" Yaya.Hedgehog.Expr (genMuExpr)
import safe "base" Prelude (toInteger)

-- TODO: For some reason HLint is complaining that TemplateHaskell is unused.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    ( assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
        =<< forAll (Gen.sized genMuExpr)
    )

-- | Checks the laws.
--
-- @since 0.2.1.3
tests :: IO Bool
tests = checkParallel $$discover
