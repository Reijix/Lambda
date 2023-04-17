{-# LANGUAGE ImportQualifiedPost #-}

module Lexer where

import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["\\", "="]
    names = []
    style =
      emptyDef
        { Tok.commentLine = "#"
        , Tok.reservedOpNames = ops
        , Tok.reservedNames = names
        , Tok.opLetter = oneOf "\\=."
        , Tok.identStart = letter
        , Tok.identLetter = alphaNum
        }

digit :: Parser Integer
digit = Tok.natural lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

dot :: Parser String
dot = Tok.dot lexer

semi :: Parser String
semi = Tok.semi lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

newline :: Parser String
newline = Tok.symbol lexer "\n"
