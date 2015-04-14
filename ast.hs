module Ast where

type Id = String

data Exp = Abs Id Exp
         | Var Id
         | App Exp Exp
         | If Exp Exp Exp
         deriving (Eq, Ord, Show)