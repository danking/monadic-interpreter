module KonValEnv ( Frame(..)
                 , Kon
                 , halt
                 , Addr
                 , Env
                 , ExpInCtx
                 , Val(..)
                 , emptyenv
                 ) where

import qualified Ast

import qualified Control.Comonad.Env as Co
import qualified Data.Map as M

data Frame = AppL ExpInCtx
           | AppR Val
           | If ExpInCtx ExpInCtx
instance Show Frame where
  show (AppL e) = "(AppL " ++ show (Co.extract e) ++ ")"
  show (AppR v) = "(AppR " ++ show v ++ ")"
  show (If c a) =    "(If "
                  ++ show (Co.extract c) ++ " "
                  ++ show (Co.extract a)
                  ++ ")"


type Kon = [Frame]

halt :: Kon
halt = []

type Addr = Integer
type Env = M.Map Ast.Id Addr
type ExpInCtx = Co.Env (Kon, Env) Ast.Exp
data Val = Clo Ast.Id ExpInCtx
         | I Integer
         | B Bool

instance Show Val where
  show (Clo v b) =
    "(Clo " ++ show v ++ " " ++ show (Co.extract b) ++ ")"
  show (I i) = "(I " ++ show i ++ ")"
  show (B b) = "(B " ++ show b ++ ")"

emptyenv :: Env
emptyenv = M.empty
