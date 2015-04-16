module Eval where

import Prelude hiding (lookup)
import Ast
import CoAst
import Coercions
import EvalMonad
import Val

import Control.Comonad (Comonad, extend)
import Control.Comonad.Env (extract, ask)
import qualified Data.Map as M

ex :: Comonad w => w a -> a
ex = extract

eval :: EvalComonad Exp -> EvalMonad Val
eval a = case pushin a of
  CoAbs v e   -> return $ Clo (ex v) (ex e) (ask v)
  CoVar v     -> address v >>= lookup
  CoApp e₁ e₂ -> do
    f <- eval e₁
    v <- eval e₂
    apply f v a
  CoIf p c a  -> undefined

apply :: Val -> Val -> EvalComonad a -> EvalMonad Val
apply f v ctx = do
  (x, b, e) <- coerceClo f
  a <- store v
  eval $ bindLocal x a e b
