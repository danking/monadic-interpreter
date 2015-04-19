{-# LANGUAGE FlexibleInstances #-}
module Parser (parse) where

import Ast

import Lexer
import Prelude hiding (abs, exp, EQ, LT, GT)
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
exp = try bop
  <|> try app
  <|> noapp
  <?> "expression"

noapp :: MyParsec Exp
noapp = abs
    <|> var
    <|> ifte
    <|> int
    <|> wrappedExp (symbol "(") (symbol ")")
    <?> "expression"

abs :: MyParsec Exp
abs = do
  _ <- lambdaSymbol
  v <- identifier
  _ <- symbol "."
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
  _  <- symbol "if"
  e₁ <- exp
  _  <- symbol "then"
  e₂ <- exp
  _  <- symbol "else"
  e₃ <- exp
  return $ If e₁ e₂ e₃
  <?> "if"

int :: MyParsec Exp
int = fmap I integer

bop :: MyParsec Exp
bop = do
  e₁ <- noapp
  op <- binop
  e₂ <- exp
  return $ BOp op e₁ e₂

binop :: MyParsec BinOp
binop = (reserved "+"  >> return Plus)
    <|> (reserved "*"  >> return Times)
    <|> (reserved "-"  >> return Minus)
    <|> (reserved "<"  >> return LT)
    <|> (reserved "<=" >> return LTE)
    <|> (reserved "==" >> return EQ)
    <|> (reserved ">=" >> return GTE)
    <|> (reserved ">"  >> return GT)

wrappedExp :: MyParsec a -> MyParsec b -> MyParsec Exp
wrappedExp l r = do
  _ <- l
  x <- do    tw bop
         <|> tw app
         <|> tw abs
         <|> tw var
         <|> tw ifte
         <|> tw (wrappedExp (symbol "(") (symbol ")"))
  return x
  <?> "parenthesized expression"
  where tw p = try $ do { v <- p ; _ <- r ; return v }

