module Val (Addr, Env, Val(..), emptyenv) where

import Ast

import qualified Data.Map as M

type Addr = Integer
type Env = M.Map Id Addr
data Val = Clo Id Exp Env
         deriving (Eq, Show, Ord)

emptyenv :: Env
emptyenv = M.empty
