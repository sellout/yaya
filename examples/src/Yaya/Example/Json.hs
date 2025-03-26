{-# LANGUAGE Trustworthy #-}

-- | An illustration of fairly basic recursion schemes via an implementation of
--   [jq](https://jqlang.org/).
--
--   Another interesting reference might be [Chris Penner’s post on jq with
--   optics](https://chrispenner.ca/posts/traversal-systems)
module Yaya.Example.Json
  ( Filter (..),
    Json (..),
  )
where

import "base" Data.Bool (Bool)
import "base" Data.Int (Int)
import "containers" Data.Map (Map)
import "text" Data.Text (Text)
import "base" Prelude (Double)

-- | We define it this way as a common core to be shared between the recursion
--   scheme and direct encodings.
data Json
  = Null
  | Bool Bool
  | Number Double
  | String Text
  | Array [Json]
  | Object (Map Text Json)

data Filter
  = Identity
  | Optional Filter
  | ObjectIndex Text
  | ArrayIndex Int
  | Slice Int Int
  | Iterator
  | -- | The comma filter
    Fork [Filter]
  | Pipe [Filter]
  | ConstructArray [Filter]
  | -- |
    --
    --  __TODO__: Generalize the keys here to support filters
    ConstructObject [(Text, Filter)]
  | -- | `.. | .a?` - Optional (ObjectIndex "a" (RecursiveDescent Identity))
    RecursiveDescent
  | Add Filter Filter
  | Subtract Filter Filter
  | Multiply Filter Filter
  | Divide Filter Filter
  | Modulo Filter Filter
  | Abs
  | Length
