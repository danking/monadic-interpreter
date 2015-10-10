module Val (Addr, Env, Val(..), emptyenv) where

import Ast

import qualified Control.Comonad.Env as Co
import qualified Data.Map as M

type Addr = Integer
type Env = M.Map Id Addr
data Val = Clo Id (Co.Env Env Exp)

instance Show Val where
  show (Clo v b) =
    "(Clo " ++ show v ++ " " ++ show (Co.extract b) ++ ")"

emptyenv :: Env
emptyenv = M.empty
