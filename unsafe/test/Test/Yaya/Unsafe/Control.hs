{-# LANGUAGE TemplateHaskell #-}

module Test.Yaya.Unsafe.Control where

import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)
