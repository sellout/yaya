{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wwarn=missed-specialisations #-}

-- |
--
--   Lexing and parsing fall into recursion schemes in an interesting way.
--   Lexing is a /streaming/ process, which means that it can be applied to
--   either recursive or corecursive inputs, and produce a result with the same
--   properties. Parsing is also a streaming process, but the output stream
--   consists of /recursive/ structures. I.e., each AST item in the output
--   stream is finite, but the stream itself may be infinite.
module Yaya.Example.Json.Schemes.JsonParser
  ( json,
    lexeme,
    steppable,
  )
where

import "base" Control.Applicative (liftA2, pure, (*>), (<*), (<|>))
import "base" Control.Category (id, (.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Char (Char)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (foldr)
import "base" Data.Function (const, ($))
import "base" Data.Functor (fmap, void, (<$), (<$>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" GHC.Generics (Generic)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import qualified "containers" Data.Set as Set
import "megaparsec" Text.Megaparsec (Parsec)
import qualified "megaparsec" Text.Megaparsec as Parser
import qualified "megaparsec" Text.Megaparsec.Char as Char
import qualified "megaparsec" Text.Megaparsec.Char.Lexer as Lexer
import "text" Data.Text (Text)
import qualified "text" Data.Text as Text
import "yaya" Yaya.Fold (Steppable, embed)
import "this" Yaya.Example.Json.Schemes
  ( Json (Array, Bool, Null, Number, Object, String),
  )
import "base" Prelude (Double, Integer, fromIntegral, negate, toEnum, (*), (^))

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XTypeApplications
-- >>> import "yaya" Yaya.Fold (Mu)

data Lexeme
  = -- | @{@
    ObjectOpen
  | -- | @}@
    ObjectClose
  | -- | @,@
    Separator
  | -- | @:@
    KeyDelimiter
  | -- | @[@
    ArrayOpen
  | -- | @]@
    ArrayClose
  | LitString Text
  | LitNumber Double
  | LitBool Bool
  | LitNull
  | Whitespace
  deriving stock (Eq, Generic, Ord, Read, Show)

type Lexer = Parsec Void String

type Parser = Parsec Void [Lexeme]

-- | This extracts a single `Lexeme` from the stream.
--
--   Lexing doesn’t create recursive structures, so it might seem like an
--   unlikely place for recursion schemes, but it converts a stream of tokens
--   (characters in this case) into a stream of lexemes, and streams are
--   recursive. Lexing thus has some interesting properties.
--
-- - it will convert a strict token stream into a strict lexeme stream
-- - it will convert a lazy token stream into a lazy lexeme stream
-- - any subsequences that persist into the lexemes will be finite, but it’s
--  `Partial` to construct them from a lazy stream.
--
-- >>> Parser.parse (Parser.some lexeme) "" "{\"some\": \"object\"}"
-- Right [ObjectOpen,LitString "some",KeyDelimiter,Whitespace,LitString "object",ObjectClose]
lexeme :: Lexer Lexeme
lexeme =
  (ObjectOpen <$ Parser.single '{')
    <|> (ObjectClose <$ Parser.single '}')
    <|> (ArrayOpen <$ Parser.single '[')
    <|> (ArrayClose <$ Parser.single ']')
    <|> (Separator <$ Parser.single ',')
    <|> (KeyDelimiter <$ Parser.single ':')
    <|> (LitString <$> string)
    <|> (LitNumber <$> number)
    <|> (LitBool <$> (true <|> false))
    <|> (LitNull <$ null)
    <|> (Whitespace <$ whitespace)

-- |
--
-- >>> Parser.runParser (json @(Mu Json)) "" <$> Parser.runParser (Parser.some lexeme) ""  "   {\"some\": [\"array\", 42, \"of\"  , {\"more\" : \"objects\"}, null, true, false]} "
-- Right (Right (embed (Object (fromList [("some",embed (Array [embed (String "array"),embed (Number 42.0),embed (String "of"),embed (Object (fromList [("more",embed (String "objects"))])),embed Null,embed (Bool True),embed (Bool False)]))]))))
json :: (Steppable (->) t Json) => Parser t
json = element $ steppable value

-- | Convert a parser for a pattern functor into a recursive parser for an
--   entire structure.
--
--  __FIXME__: This shouldn’t require recursion, but currently does.
steppable :: (Steppable (->) t f) => (Parser t -> Parser (f t)) -> Parser t
steppable fa = go
  where
    go = embed <$> fa go

value :: Parser a -> Parser (Json a)
value val =
  (Object <$> object val)
    <|> (Array <$> array val)
    <|> (String <$> extractString)
    <|> (Number <$> extractNumber)
    <|> (Bool <$> extractBool)
    <|> (Null <$ Parser.single LitNull)

true :: Lexer Bool
true = True <$ Parser.chunk "true"

false :: Lexer Bool
false = False <$ Parser.chunk "false"

null :: Lexer ()
null = void $ Parser.chunk "null"

object :: Parser a -> Parser (Map Text a)
object val =
  fmap Map.fromList $
    Parser.single ObjectOpen *> members val <* Parser.single ObjectClose

members :: Parser a -> Parser [(Text, a)]
members = (`Parser.sepBy` Parser.single Separator) . member

extractString :: Parser Text
extractString =
  Parser.token
    ( \case
        LitString str -> pure str
        _ -> Nothing
    )
    Set.empty

extractBool :: Parser Bool
extractBool =
  Parser.token
    ( \case
        LitBool b -> pure b
        _ -> Nothing
    )
    Set.empty

extractNumber :: Parser Double
extractNumber =
  Parser.token
    ( \case
        LitNumber num -> pure num
        _ -> Nothing
    )
    Set.empty

member :: Parser a -> Parser (Text, a)
member val =
  liftA2 (,) (ws *> extractString <* ws) $
    Parser.single KeyDelimiter *> element val

array :: Parser a -> Parser [a]
array val =
  Parser.single ArrayOpen *> elements val <* Parser.single ArrayClose

elements :: Parser a -> Parser [a]
elements = (`Parser.sepBy` Parser.single Separator) . element

element :: Parser a -> Parser a
element val = ws *> val <* ws

string :: Lexer Text
string = fmap Text.pack $ Parser.single '"' *> characters <* Parser.single '"'

characters :: Lexer [Char]
characters = Parser.many character

-- |
--
--  __FIXME__: This is too broad.
character :: Lexer Char
character = (Parser.single '\\' *> escape) <|> Parser.anySingleBut '"'

escape :: Lexer Char
escape =
  Parser.oneOf ['"', '\\', '/']
    <|> ('\b' <$ Parser.single 'b')
    <|> ('\f' <$ Parser.single 'f')
    <|> ('\n' <$ Parser.single 'n')
    <|> ('\r' <$ Parser.single 'r')
    <|> ('\t' <$ Parser.single 't')
    <|> (toEnum <$> (Parser.single 'u' *> Lexer.hexadecimal))

number :: Lexer Double
number = do
  int <- integer
  _frac <- Parser.optional fraction
  exp <- Parser.optional exponent
  pure . fromIntegral $ foldr (\e i -> i * e ^ (10 :: Integer)) int exp

integer :: Lexer Integer
integer = do
  negative <- Parser.optional $ Parser.single '-'
  magnitude <- digits
  pure $ foldr (const negate) (fromIntegral magnitude) negative

digits :: Lexer Natural
digits = Lexer.decimal

fraction :: Lexer Natural
fraction = Parser.single '.' *> digits

exponent :: Lexer Integer
exponent = do
  _ <- Char.char' 'e'
  sign <-
    Parser.optional $
      (negate <$ Parser.single '-') <|> (id <$ Parser.single '+')
  magnitude <- digits
  pure $ foldr ($) (fromIntegral magnitude) sign

ws :: Parser ()
ws = void $ Parser.many (Parser.single Whitespace)

whitespace :: Lexer ()
whitespace =
  void . Parser.some $ Parser.oneOf ['\x0020', '\x000a', '\x000d', '\x0009']
