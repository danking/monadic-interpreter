module Eval where

import Prelude hiding (lookup)
import Ast
import Coercions
import EvalMonad
import Val

import Control.Comonad (extend)
import Control.Comonad.Env (extract, ask)
import qualified Data.Map as M

eval :: EvalComonad Exp -> EvalMonad Val
eval a = case extract a of
  Abs v e   -> return $ Clo v e $ ask a
  Var v     -> address v a >>= lookup
  App e₁ e₂ -> do
    f <- eval (extend (const e₁) a)
    v <- eval (extend (const e₂) a)
    apply f v a
  If p c a  -> undefined

apply :: Val -> Val -> EvalComonad a -> EvalMonad Val
apply f v ctx = do
  (x, b, e) <- coerceClo f
  a <- store v
  eval $ bindLocal x a e b
