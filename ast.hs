module Ast where

type Id = String

data BinOp = Plus
           | Minus
           | Times
           | LT
           | GT
           | LTE
           | GTE
           deriving (Eq, Ord, Show)

data Exp = Abs Id Exp
         | Var Id
         | App Exp Exp
         | BOp BinOp Exp Exp
         | If Exp Exp Exp
         deriving (Eq, Ord, Show)
