module Coercions where

import Ast
import Val
import EvalMonad

coerceClo :: Val -> EvalMonad (Id, Exp, Env)
coerceClo v = case v of
  Clo x b e -> return (x, b, e)
