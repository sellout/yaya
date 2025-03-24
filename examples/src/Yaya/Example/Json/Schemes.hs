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

import safe "base" Control.Applicative (pure)
import safe "base" Control.Arrow ((&&&))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (Monad, join, (<=<), (=<<))
import safe "base" Data.Bitraversable (bisequence)
import safe "base" Data.Either (Either (Left), fromRight)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (foldr, notElem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Int (Int)
import safe "base" Data.List ((!!))
import safe qualified "base" Data.List as List
import safe "base" Data.Maybe (Maybe, maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Traversable (sequenceA, traverse)
import safe "base" Data.Tuple (curry, uncurry)
import safe "base" Numeric.Natural (Natural)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe qualified "bytestring" Data.ByteString as ByteString
import safe qualified "containers" Data.Map as Map
import safe "deriving-compat" Data.Eq.Deriving (deriveEq1)
import safe "deriving-compat" Data.Ord.Deriving (deriveOrd1)
import safe "deriving-compat" Text.Read.Deriving (deriveRead1)
import safe "deriving-compat" Text.Show.Deriving (deriveShow1)
import safe "text" Data.Text (Text)
import safe qualified "text" Data.Text as Text
import safe "text" Data.Text.Encoding (encodeUtf8)
import safe "yaya" Yaya.Fold (Steppable, embed, project)
import safe qualified "yaya-native" Yaya.Native.Retrofit as Retrofit
import safe qualified "this" Yaya.Example.Json as Direct
import safe "base" Prelude
  ( Double,
    Integer,
    fromIntegral,
    mod,
    truncate,
    (*),
    (+),
    (-),
    (/),
  )
import safe qualified "base" Prelude as Base

-- This is not the recommended way to define a pattern functor. This is only
-- done to show how we can use the same origirnal representation as the direct
-- approach, but get the benefits of recursion schemes without rewriting
-- existing code.
Retrofit.extractPatternFunctor Retrofit.qualifiedRules ''Direct.Filter
Retrofit.extractPatternFunctor Retrofit.qualifiedRules ''Direct.Json

deriving stock instance (Eq a) => Eq (Filter a)

deriving stock instance (Ord a) => Ord (Filter a)

deriving stock instance (Read a) => Read (Filter a)

deriving stock instance (Show a) => Show (Filter a)

deriveEq1 ''Filter
deriveOrd1 ''Filter
deriveRead1 ''Filter
deriveShow1 ''Filter

deriving stock instance (Eq a) => Eq (Json a)

deriving stock instance (Ord a) => Ord (Json a)

deriving stock instance (Read a) => Read (Json a)

deriving stock instance (Show a) => Show (Json a)

deriveEq1 ''Json
deriveOrd1 ''Json
deriveRead1 ''Json
deriveShow1 ''Json

-- * filters

-- | Extract an entry from an object. If the object is missing the key, return
--  `None`, but if the argument isn’t an object, return `Left`.
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
  (Array a, Array b) -> pure . Array $ List.filter (`notElem` b) a
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

abs :: Json a -> Either String Double
abs = \case
  -- FIXME: I’m not sure if `truncate` is the right operation here, see what
  --        @jq@ does.
  Number n -> pure $ Base.abs n
  _ -> Left "attempted to take the abs of a non-number"

-- |
--
-- >>> length (String "too long")
-- Right 8
length :: Json a -> Either String Natural
length = \case
  String text -> pure . fromIntegral $ Text.length text
  -- FIXME: I’m not sure if `truncate` is the right operation here, see what
  --        @jq@ does.
  Number n -> pure . truncate $ Base.abs n
  Array elements -> pure . fromIntegral $ List.length elements
  Object entries -> pure . fromIntegral $ Map.size entries
  Null -> pure 0
  Bool _ -> Left "attempted to take the length of a bool"

utf8ByteLength :: Json a -> Either String Natural
utf8ByteLength = \case
  String text -> pure . fromIntegral . ByteString.length $ encodeUtf8 text
  _ -> Left "attempted to get the utf8bytelength of a non-string"

-- | Performs a binary operation on the results of two `jq` filters.
--
--  __FIXME__: Not sure how @jq@ handles filters that return multiple results –
--             Cartesian product? zip?
binop ::
  (Monad m) =>
  (Json a -> Json a -> m (Json a)) ->
  (Json a -> m [Json a]) ->
  (Json a -> m [Json a]) ->
  Json a ->
  m [Json a]
binop op filtA filtB =
  traverse (uncurry op) . bisequence <=< bisequence . (filtA &&& filtB)

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XTypeApplications
-- >>> import "yaya" Yaya.Fold (Mu, cata)

-- | Convert a `Filter` to a function that filters the provided JSON structure.
--
-- >>> let json = Object (Map.fromList [("name", embed (String "bob")), ("title", embed (String "boss"))])
-- >>> cata @_ @(Mu Filter) (filter @(Mu Json)) (embed (ObjectIndex "title")) json
-- Right [String "boss"]
-- >>> cata @_ @(Mu Filter) (filter @(Mu Json)) (embed (Pipe [embed (ObjectIndex "title"), embed (Slice 1 3)])) json
-- Right [String "os"]
-- >>> cata @_ @(Mu Filter) (filter @(Mu Json)) (embed Iterator) json
-- Right [String "bob",String "boss"]
-- >>> cata @_ @(Mu Filter) (filter @(Mu Json)) (embed (Pipe [embed Iterator, embed (Slice 1 3)])) json
-- Right [String "ob",String "os"]
filter ::
  (Eq json, Steppable (->) json Json) =>
  Filter (Json json -> Either String [Json json]) ->
  Json json ->
  Either String [Json json]
filter = \case
  Identity -> pure . pure
  Optional filt -> pure . fromRight [] . filt
  ObjectIndex key -> fmap (maybe [Null] (pure . project)) . objectIndex key
  ArrayIndex index -> fmap (maybe [Null] (pure . project)) . arrayIndex index
  Slice start end -> fmap pure . slice start end
  Iterator -> fmap (fmap project) . iterator
  Fork filts -> fmap join . sequenceA . sequenceA filts
  Pipe filts ->
    foldr (\prev next -> fmap join . traverse next <=< prev) (pure . pure) filts
  ConstructArray filts ->
    fmap (pure . Array . fmap embed . join) . sequenceA . sequenceA filts
  ConstructObject filts ->
    fmap (pure . Object . fmap embed . Map.fromList . (sequenceA =<<))
      . traverse sequenceA
      . traverse sequenceA filts
  Add filtA filtB -> binop add filtA filtB
  Subtract filtA filtB -> binop subtract filtA filtB
  Multiply filtA filtB -> binop multiply filtA filtB
  Divide filtA filtB -> binop divide filtA filtB
  Modulo filtA filtB -> binop modulo filtA filtB
  Abs -> fmap (pure . Number) . abs
  Length -> fmap (pure . Number . fromIntegral) . length
