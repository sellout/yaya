{-# LANGUAGE TemplateHaskell #-}

module Test.Fold.Common where

import Control.Category (Category (..))
import Control.Monad ((=<<))
import Data.Bool (Bool)
import Data.Functor (Functor (..))
import Data.Ord (Ord (..))
import Hedgehog (Property, assert, checkParallel, discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import System.IO (IO)
import Yaya.Fold (Recursive (..), zipAlgebras)
import Yaya.Fold.Common (height, size)
import Yaya.Hedgehog.Expr (genMuExpr)
import Yaya.Pattern (uncurry)
import Prelude (Integral (..))

prop_heightLtSize :: Property
prop_heightLtSize =
  property
    ( assert . uncurry (<) . fmap toInteger . cata (zipAlgebras height size)
        =<< forAll (Gen.sized genMuExpr)
    )

tests :: IO Bool
tests = checkParallel $$(discover)
