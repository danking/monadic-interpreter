module Run (run, run') where

import Ast
import Eval
import EvalMonad
import Val

run :: Exp -> Either Failure Val
run = runEvalMonad . eval . injectProg

run' :: Exp -> Either Failure (Val, (Addr, Store))
run' = runEvalMonad' . eval . injectProg
