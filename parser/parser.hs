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
exp = try app
  <|> try bop
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
  return $ rotateApp e₁ e₂
  <?> "application"

-- To avoid left-recursion, app is right-associatively parsed. We undo that with
-- `rotateApp`
rotateApp :: Exp -> Exp -> Exp
rotateApp e₁ e = case e of
  App e₂ e₃ -> rotateApp (App e₁ e₂) e₃
  e₂        -> App e₁ e₂

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

int :: MyParsec Exp
int = fmap I integer

bop :: MyParsec Exp
bop = do
  e₁ <- noapp
  op <- operator
  e₂ <- exp
  return $ BOp (binopFromString op) e₁ e₂

binopFromString :: String -> BinOp
binopFromString o = case o of
  "+"  -> Plus
  "*"  -> Times
  "-"  -> Minus
  "<"  -> LT
  "<=" -> LTE
  "==" -> EQ
  ">=" -> GTE
  ">"  -> GT

wrappedExp :: MyParsec a -> MyParsec b -> MyParsec Exp
wrappedExp l r = do
  l
  x <- do    tw app
         <|> tw bop
         <|> tw abs
         <|> tw var
         <|> tw ifte
         <|> tw (wrappedExp (symbol "(") (symbol ")"))
  return x
  <?> "parenthesized expression"
  where tw p = try $ do { v <- p ; r ; return v }

