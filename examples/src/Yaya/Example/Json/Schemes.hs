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
  ( abs,
    add,
    and,
    divide,
    filter,
    keys,
    keysUnsorted,
    length,
    modulo,
    multiply,
    not,
    or,
    subtract,
    utf8ByteLength,
    wrapBinop,
    wrapUnop,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Arrow ((&&&))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (Monad, join, (<=<), (=<<))
import safe "base" Data.Bifunctor (bimap)
import safe "base" Data.Bitraversable (bisequence)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either, fromRight)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (foldr, notElem)
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap, (<$>))
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
import safe "yaya" Yaya.Fold
  ( Recursive,
    Steppable,
    cata,
    distTuple,
    embed,
    gcata,
    project,
  )
import safe "yaya" Yaya.Pattern (fst, snd)
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

-- | This basically applies `iterator` recursively, collecting everything into a
--   single list.
recursiveDescent ::
  (Recursive (->) json Json, Steppable (->) json Json) => json -> [Json json]
recursiveDescent =
  gcata (distTuple embed) \orig ->
    let self = fmap fst orig
     in either (const [self]) (self :) . iterator <=< sequenceA $ fmap snd orig

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

multiply :: Json a -> Json a -> Either String Double
multiply = curry \case
  (Number a, Number b) -> pure $ a * b
  (_, _) -> Left "attempted to multiply non-numbers"

divide :: Json a -> Json a -> Either String Double
divide = curry \case
  (Number a, Number b) -> pure $ a / b
  (_, _) -> Left "attempted to divide non-numbers"

modulo :: Json a -> Json a -> Either String Integer
modulo = curry \case
  (Number a, Number b) -> pure $ truncate a `mod` truncate b
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

keys :: Json a -> Either String [Json a]
keys = \case
  Array elements -> pure $ Number . fromIntegral <$> [0 .. List.length elements]
  Object entries -> pure . fmap String . List.sort $ Map.keys entries
  _ -> Left "attempted to get the keys of a non-structure"

keysUnsorted :: Json a -> Either String [Json a]
keysUnsorted = \case
  Array elements -> pure $ Number . fromIntegral <$> [0 .. List.length elements]
  Object entries -> pure $ String <$> Map.keys entries
  _ -> Left "attempted to get the keys of a non-structure"

and :: Json a -> Json a -> Bool
and = curry \case
  (Null, _) -> False
  (Bool False, _) -> False
  (_, Null) -> False
  (_, Bool False) -> False
  (_, _) -> True

or :: Json a -> Json a -> Bool
or = curry \case
  (Null, Null) -> False
  (Null, Bool False) -> False
  (Bool False, Null) -> False
  (Bool False, Bool False) -> False
  (_, _) -> True

not :: Json a -> Bool
not = \case
  Null -> True
  Bool False -> True
  _ -> False

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
-- >>> :{
--   let json = Object @(Mu Json) . Map.fromList $
--         [("name", embed (String "bob")), ("title", embed (String "boss"))]
-- :}
--
-- >>> cata @_ @(Mu Filter) filter (embed (ObjectIndex "title")) json
-- Right [String "boss"]
-- >>> cata @_ @(Mu Filter) filter (embed (Pipe [embed (ObjectIndex "title"), embed (Slice 1 3)])) json
-- Right [String "os"]
-- >>> cata @_ @(Mu Filter) filter (embed Iterator) json
-- Right [String "bob",String "boss"]
-- >>> cata @_ @(Mu Filter) filter (embed (Pipe [embed Iterator, embed (Slice 1 3)])) json
-- Right [String "ob",String "os"]
-- >>> cata @_ @(Mu Filter) filter (embed RecursiveDescent) json
-- Right [Object (fromList [("name",embed (String "bob")),("title",embed (String "boss"))]),String "bob",String "boss"]
--
--   There are two ways to view operations that operate on multiple recursive
--   structures:
-- - as functions that move through both structures together or
-- - as functions that traverse one of the structures to build a function that
--   steps through the other structures.
--
--   The latter is how the implementations actually, work, but the former is
--   often a helpful mental model. In some cases (not `filter`) there are ways
--   to implement it that make the former more apparent.
--
--   In the case of @`cata` `filter`@, we fold over the `Filter` fixed point to
--   build a function that peels the `Json` structure as much as necessary. In
--   most cases, each `Filter` peels one layer of `Json`, and the former view
--   maps well. But, for example, in the `RecursiveDescent` case, we actually
--   have to perform a fold over the `Json` structure, which exposes the falsity
--   of that view.
filter ::
  (Recursive (->) json Json, Steppable (->) json Json) =>
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
  RecursiveDescent -> pure . recursiveDescent . embed
  -- See the docs on `wrapBinop`.
  Binop op filtA filtB ->
    binop
      ( curry $
          fmap (project . cata embed)
            . uncurry op
            . bimap (cata embed . embed) (cata embed . embed)
      )
      filtA
      filtB
  -- See the docs on `wrapUnop`.
  Unop op -> fmap (pure . project . cata embed) . op . cata embed . embed

-- | Because `Json` is extracted from `Direct.Json`, this is needed to be able
--   to pass our pattern functor operations (like `add`) to `Binop`.
wrapBinop ::
  (Json Direct.Json -> Json Direct.Json -> Either String (Json Direct.Json)) ->
  a ->
  a ->
  Filter a
wrapBinop op = Binop . curry $ fmap embed . uncurry op . bimap project project

-- | Because `Json` is extracted from `Direct.Json`, this is needed to be able
--   to pass our pattern functor operations (like `abs`) to `Unop`.
wrapUnop ::
  (Json Direct.Json -> Either String (Json Direct.Json)) -> Filter a
wrapUnop op = Unop $ fmap embed . op . project
