module Parser (parseLine, readP, parsePrelude) where

import Data.Either.Extra (eitherToMaybe)
import Lexer (digit, dot, identifier, parens, reservedOp, semi, whiteSpace)
import Syntax (Declaration (..), Term (..))
import Text.Parsec (
  ParseError,
  endBy,
  eof,
  many,
  many1,
  newline,
  parse,
  sepBy,
  try,
  (<?>),
  (<|>),
 )
import Text.Parsec.String (Parser)
import Prelude hiding (abs)

line :: Parser (Either Declaration Term)
line = try (Left <$> declaration) <|> try (Right <$> term)

declaration :: Parser Declaration
declaration =
  Declaration
    <$> identifier
    <*> (reservedOp "=" *> term)
    <?> "declaration"

term :: Parser Term
term = abs <|> app <|> numeral <|> var

atomicTerm :: Parser Term
atomicTerm = abs <|> numeral <|> var <|> (parens term <?> "nested term")

var :: Parser Term
var =
  Var
    <$> identifier

numeral :: Parser Term
numeral =
  churchEncode
    <$> digit

churchEncode :: Integer -> Term
churchEncode n = Abs "f" (Abs "a" (go n))
  where
    go 0 = Var "a"
    go n = App (Var "f") (go (n - 1))

app :: Parser Term
app = foldl1 App <$> many1 atomicTerm

abs :: Parser Term
abs =
  Abs
    <$> (reservedOp "\\" *> identifier)
    <*> (dot *> term)
    <?> "lambda abstraction"

-- runners
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

parseLine :: String -> Either ParseError (Either Declaration Term)
parseLine = Text.Parsec.parse (contents line) "stdin"

parsePrelude :: String -> Either ParseError [Declaration]
parsePrelude = Text.Parsec.parse (contents (declaration `endBy` semi)) "prelude.lmd"

readP :: String -> Maybe Term
readP line = eitherToMaybe (Text.Parsec.parse (contents term) "sourcefile" line)
