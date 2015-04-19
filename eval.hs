module Eval where

import Prelude hiding (lookup, LT, GT, EQ)
import Ast
import CoAst
import Coercions
import EvalMonad
import HSVal
import Val

import Control.Comonad (Comonad)
import Control.Comonad.Env (extract)

ex :: Comonad w => w a -> a
ex = extract

eval :: EvalComonad Exp -> EvalMonad Val
eval a = case pushin a of
  CoAbs v e   -> return $ Clo (ex v) e
  CoVar v     -> address v >>= lookup
  CoApp e₁ e₂ -> do
    f <- eval e₁
    v <- eval e₂
    apply f v
  CoBOp b e₁ e₂ -> do
    v₁ <- eval e₁
    v₂ <- eval e₂
    evalOp b v₁ v₂
  CoI i ->
    return $ Val.I i
  CoIf p c a -> do
    b <- coerceBool =<< eval p
    if b
      then eval c
      else eval a

apply :: Val -> Val -> EvalMonad Val
apply f v = do
  (x, b) <- coerceClo f
  a <- store v
  eval $ bindLocal x a b

evalOp :: BinOp -> Val -> Val -> EvalMonad Val
evalOp b = case b of
  Plus  -> liftBinOp ((+) :: Integer -> Integer -> Integer)
  Minus -> liftBinOp ((-) :: Integer -> Integer -> Integer)
  Times -> liftBinOp ((*) :: Integer -> Integer -> Integer)
  LT  -> liftBinOp ((<)  :: Integer -> Integer -> Bool)
  GT  -> liftBinOp ((>)  :: Integer -> Integer -> Bool)
  LTE -> liftBinOp ((<=) :: Integer -> Integer -> Bool)
  GTE -> liftBinOp ((>=) :: Integer -> Integer -> Bool)
  EQ  -> liftBinOp ((==) :: Integer -> Integer -> Bool)

liftBinOp :: ValLike a => ValLike b =>
             (a -> a -> b) -> Val -> Val -> EvalMonad Val
liftBinOp bop v₁ v₂ = do
  n₁ <- prj v₁
  n₂ <- prj v₂
  return $ inj $ n₁ `bop` n₂
