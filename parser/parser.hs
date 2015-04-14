{-# LANGUAGE FlexibleInstances #-}
module Parser (parse) where

import Ast

import Control.Monad (MonadPlus, mzero, mplus)
import Data.Foldable (Foldable, asum, foldMap)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Traversable (Traversable, traverse)
import Lexer
import Prelude hiding (abs, exp)
import Text.Parsec hiding (parse)

import Debug.Trace

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
  terminatedBy eof exp

exp :: MyParsec Exp
exp =   abs
    <|> var
    <|> ifte
    <|> wrappedExp (symbol "(") (symbol ")")
    <|> app
    <?> "expression"

abs :: MyParsec Exp
abs = do
  (trace "lambda" lambdaSymbol)
  v <- identifier
  (trace ("  v is " ++ show v) (symbol "."))
  e <- exp
  _ <- (trace ("  e is " ++ show e) $ return ())
  return $ Abs v e
  <?> "lambda abstraction"

app :: MyParsec Exp
app = do
  _ <-  trace "before app1" (return ())
  e₁ <- exp
  _ <-  trace "after app1" (return ())
  e₂ <- exp
  return $ App e₁ e₂
  <?> "application"

var :: MyParsec Exp
var = do
  v <- identifier
  return $ trace ("var is " ++ v) $ Var v
  <?> "variable reference"

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
  do     twt abs
     <|> twt var
     <|> twt ifte
     <|> twt (wrappedExp l r)
     <|> twt app
  <?> "parenthesized expression"
  where twt = terminatedBy r

terminatedBy :: MyParsec a -> MyParsec Exp -> MyParsec Exp
terminatedBy t p = try $ do
  x <- p
  t
  return x
