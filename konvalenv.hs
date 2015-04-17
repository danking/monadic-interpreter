module KonValEnv ( Frame(..)
                 , Kon
                 , halt
                 , Addr
                 , Env
                 , ExpInCtx
                 , Val(..)
                 , emptyenv
                 ) where

import Ast

import qualified Control.Comonad.Env as Co
import qualified Data.Map as M

data Frame = AppL Exp
           | AppR Val
           | If Exp Exp
           deriving (Show)

type Kon = [Frame]

halt :: Kon
halt = []

type Addr = Integer
type Env = M.Map Id Addr
type ExpInCtx = Co.Env (Kon, Env) Exp
data Val = Clo Id ExpInCtx

instance Show Val where
  show (Clo v b) =
    "(Clo " ++ show v ++ " " ++ show (Co.extract b) ++ ")"

emptyenv :: Env
emptyenv = M.empty
