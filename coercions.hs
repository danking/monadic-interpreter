module Coercions ( coerceClo
                 , coerceInt
                 , coerceBool
                 ) where

import Ast
import Val
import EvalMonad

coerceClo :: Val -> EvalMonad (Id, ExpInCtx)
coerceClo v = case v of
  Clo x b -> return (x, b)
  x -> failM $ "needed a closure, but found a " ++ show x

coerceInt :: Val -> EvalMonad Integer
coerceInt v = case v of
  Val.I n -> return n
  x -> failM $ "needed an int, but found a " ++ show x

coerceBool :: Val -> EvalMonad Bool
coerceBool v = case v of
  B b -> return b
  x -> failM $ "needed a boolean, but found a " ++ show x
