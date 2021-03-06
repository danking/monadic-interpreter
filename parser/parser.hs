{-# LANGUAGE FlexibleInstances #-}
module Parser (parse) where

import Ast

import Lexer
import Prelude hiding (abs, exp)
import Text.Parsec hiding (parse)

-- e ∈ E ::= λx.e
--        |  e e
--        |  x
--        |  (e)
--        |  if e then e else e

parse :: String -> String -> Either ParseError Exp
parse s name = runParser prog () name s

type MyParsec = Parsec String ()

prog :: MyParsec Exp
prog = do
  whitespace
  e <- exp
  eof
  return e

exp :: MyParsec Exp
exp = try app
  <|> noapp
  <?> "expression"

noapp :: MyParsec Exp
noapp = abs
    <|> var
    <|> ifte
    <|> wrappedExp (symbol "(") (symbol ")")
    <?> "expression"

abs :: MyParsec Exp
abs = do
  lambdaSymbol
  v <- identifier
  (symbol ".")
  e <- exp
  return $ Abs v e
  <?> "lambda abstraction"

app :: MyParsec Exp
app = do
  e₁ <- noapp
  e₂ <- exp
  return $ App e₁ e₂
  <?> "application"

var :: MyParsec Exp
var =  Var `fmap` identifier <?> "variable reference"

ifte :: MyParsec Exp
ifte = do
  symbol "if"
  e₁ <- exp
  symbol "then"
  e₂ <- exp
  symbol "else"
  e₃ <- exp
  return $ If e₁ e₂ e₃
  <?> "if"

wrappedExp :: MyParsec a -> MyParsec b -> MyParsec Exp
wrappedExp l r = do
  l
  x <- do    tw app
         <|> tw abs
         <|> tw var
         <|> tw ifte
         <|> tw (wrappedExp (symbol "(") (symbol ")"))
  return x
  <?> "parenthesized expression"
  where tw p = try $ do { v <- p ; r ; return v }

