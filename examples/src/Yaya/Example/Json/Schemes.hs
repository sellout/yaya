{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- `Retrofit.extractPatternFunctor` creates instances for the type it extracts
-- from
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=missed-specialisations #-}

-- | An illustration of fairly basic recursion schemes via an implementation of
--   [jq](https://jqlang.org/).
--
--   Another interesting reference might be [Chris Penner’s post on jq with
--   optics](https://chrispenner.ca/posts/traversal-systems)
module Yaya.Example.Json.Schemes
  ( filter,
    utf8ByteLength,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Arrow ((&&&))
import "base" Control.Category ((.))
import "base" Control.Monad (join, (<=<))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bitraversable (bisequence)
import "base" Data.Either (Either (Left), fromRight)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (foldr, notElem)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap)
import "base" Data.Int (Int)
import "base" Data.List ((!!))
import qualified "base" Data.List as List
import "base" Data.Maybe (Maybe, maybe)
import "base" Data.Semigroup ((<>))
import "base" Data.String (String)
import "base" Data.Traversable (sequenceA, traverse)
import "base" Data.Tuple (curry, uncurry)
import "base" Numeric.Natural (Natural)
import qualified "bytestring" Data.ByteString as ByteString
import qualified "containers" Data.Map as Map
import "text" Data.Text (Text)
import qualified "text" Data.Text as Text
import "text" Data.Text.Encoding (encodeUtf8)
import "yaya" Yaya.Fold (Steppable, embed, project)
import qualified "yaya" Yaya.Retrofit as Retrofit
import qualified "this" Yaya.Example.Json as Direct
import "base" Prelude
  ( Integer,
    abs,
    fromIntegral,
    mod,
    truncate,
    (*),
    (+),
    (-),
    (/),
  )

-- This is not the recommended way to define a pattern functor. This is only
-- done to show how we can use the same origirnal representation as the direct
-- approach, but get the benefits of recursion schemes without rewriting
-- existing code.
Retrofit.extractPatternFunctor Retrofit.qualifiedRules ''Direct.Filter
Retrofit.extractPatternFunctor Retrofit.qualifiedRules ''Direct.Json

-- * filters

objectIndex :: Text -> Json a -> Either String (Maybe a)
objectIndex key = \case
  Object entries -> pure $ Map.lookup key entries
  _ -> Left "applied “.[<string>]” to a non-object"

arrayIndex :: Int -> Json a -> Either String (Maybe a)
arrayIndex index = \case
  -- FIXME: This should use `!?`, but it’s not supported yet.
  Array elements -> pure . pure $ elements !! index
  _ -> Left "applied “.[<number>]” to a non-array"

slice :: Int -> Int -> Json a -> Either String (Json a)
slice start end = \case
  Array elements ->
    pure . Array . List.take (end - start) $ List.drop start elements
  String text -> pure . String . Text.take (end - start) $ Text.drop start text
  _ ->
    Left
      "applied “.[<number>:<number>]” to something other than an array or string."

iterator :: Json a -> Either String [a]
iterator = \case
  Array elements -> pure elements
  Object entries -> pure $ Map.elems entries
  _ -> Left "applied “.[]” to something other than an array or object"

add :: Json a -> Json a -> Either String (Json a)
add = curry \case
  (Null, b) -> pure b
  (a, Null) -> pure a
  (Number a, Number b) -> pure . Number $ a + b
  (Array a, Array b) -> pure . Array $ a <> b
  (String a, String b) -> pure . String $ a <> b
  (Object a, Object b) -> pure . Object $ Map.union b a
  (_, _) -> Left "attempted to add two non-addable things"

subtract :: (Eq a) => Json a -> Json a -> Either String (Json a)
subtract = curry \case
  (Number a, Number b) -> pure . Number $ a - b
  (Array a, Array b) -> pure . Array $ List.filter (flip notElem b) a
  (_, _) -> Left "attempted to subtract two non-subtractable things"

multiply :: Json a -> Json a -> Either String (Json a)
multiply = curry \case
  (Number a, Number b) -> pure . Number $ a * b
  (_, _) -> Left "attempted to multiply non-numbers"

divide :: Json a -> Json a -> Either String (Json a)
divide = curry \case
  (Number a, Number b) -> pure . Number $ a / b
  (_, _) -> Left "attempted to divide non-numbers"

modulo :: Json a -> Json a -> Either String (Json a)
modulo = curry \case
  (Number a, Number b) -> pure . Number . fromIntegral @Integer $ truncate a `mod` truncate b
  (_, _) -> Left "attempted to modulo non-numbers"

length :: Json a -> Either String Natural
length = \case
  String text -> pure . fromIntegral $ Text.length text
  -- FIXME: I’m not sure if `truncate` is the right operation here, see what
  --        @jq@ does.
  Number n -> pure . truncate $ abs n
  Array elements -> pure . fromIntegral $ List.length elements
  Object entries -> pure . fromIntegral $ Map.size entries
  Null -> pure $ 0
  Bool _ -> Left "attempted to take the length of a bool"

utf8ByteLength :: Json a -> Either String Natural
utf8ByteLength = \case
  String text -> pure . fromIntegral . ByteString.length $ encodeUtf8 text
  _ -> Left "attempted to get the utf8bytelength of a non-string"

-- |
--
--  __FIXME__: Not sure how @jq@ handles filters that return multiple results –
--             Cartesian product? zip?
binop ::
  (Steppable (->) json Json) =>
  (Json json -> Json json -> Either String (Json json)) ->
  (json -> Either String [json]) ->
  (json -> Either String [json]) ->
  json ->
  Either String [json]
binop op filtA filtB =
  traverse (fmap embed . uncurry op . bimap project project) . bisequence
    <=< bisequence . (filtA &&& filtB)

-- | Convert a `Filter` to a function that filters the provided JSON structure.
filter ::
  (Eq json, Steppable (->) json Json) =>
  Filter (json -> Either String [json]) ->
  json ->
  Either String [json]
filter = \case
  Identity -> pure . pure
  Optional filt -> pure . fromRight [] . filt
  ObjectIndex key -> fmap (maybe [embed Null] pure) . objectIndex key . project
  ArrayIndex index -> fmap (maybe [embed Null] pure) . arrayIndex index . project
  Slice start end -> fmap (pure . embed) . slice start end . project
  Iterator -> iterator . project
  Fork filts -> fmap join . sequenceA . sequenceA filts
  Pipe filts ->
    foldr (\prev next -> fmap join . traverse next <=< prev) (pure . pure) filts
  ConstructArray filts ->
    fmap (pure . embed . Array . join) . sequenceA . sequenceA filts
  Add filtA filtB -> binop add filtA filtB
  Subtract filtA filtB -> binop subtract filtA filtB
  Multiply filtA filtB -> binop multiply filtA filtB
  Divide filtA filtB -> binop divide filtA filtB
  Modulo filtA filtB -> binop modulo filtA filtB
  Length -> fmap (pure . embed . Number . fromIntegral) . length . project
