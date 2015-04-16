module Coercions where

import Ast
import Val
import EvalMonad

import qualified Control.Comonad.Env as Co

coerceClo :: Val -> EvalMonad (Id, (Co.Env Env Exp))
coerceClo v = case v of
  Clo x b -> return (x, b)
