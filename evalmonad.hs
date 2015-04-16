module EvalMonad ( address
                 , bindLocal
                 , injectProg
                 , lookup
                 , runEvalMonad
                 , runEvalMonad'
                 , store
                 , EvalMonad
                 , EvalComonad
                 , Failure
                 , Store
                 ) where

import Ast
import Val

import Control.Comonad.Env hiding (Env)
import qualified Control.Comonad.Identity as Co
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Identity
import qualified Data.Map as M
import Prelude hiding (lookup)

type Store = M.Map Addr Val
type EvalComonad = EnvT Env Co.Identity
type Failure = String
type EvalMonad = StateT (Addr, Store) (Either Failure)

injectProg :: Exp -> EvalComonad Exp
injectProg = env emptyenv

emptystate :: (Addr, Store)
emptystate = (0, M.empty)

runEvalMonad :: EvalMonad a -> Either Failure a
runEvalMonad ma = fst `fmap` runStateT ma emptystate

runEvalMonad' :: EvalMonad a -> Either Failure (a, (Addr, Store))
runEvalMonad' ma = runStateT ma emptystate


getA :: EvalMonad Addr
getA = fst `fmap` get
putA :: Addr -> EvalMonad ()
putA a = do
  s <- getS
  put (a,s)

getS :: EvalMonad Store
getS = snd `fmap` get
putS :: Store -> EvalMonad ()
putS s = do
  a <- getA
  put (a,s)

failM :: String -> EvalMonad a
failM = lift . Left

maybeFails :: String -> Maybe a -> EvalMonad a
maybeFails msg ma = case ma of
  Nothing -> failM msg
  Just a  -> return a

bindLocal :: Id -> Addr -> Env -> a -> EvalComonad a
bindLocal x a e b =
  local bind (env e b)
  where bind = M.insert x a

store :: Val -> EvalMonad Addr
store v = do
  a <- getA
  s <- getS
  putS $ M.insert a v s
  putA (a + 1)
  return a

address :: Id -> EvalComonad a -> EvalMonad Addr
address id ctx = kill $ asks (M.lookup id) ctx
  where kill = maybeFails ("no environment mapping for " ++ show id)

lookup :: Addr -> EvalMonad Val
lookup addr = (M.lookup addr) `fmap` getS >>= kill
  where kill = maybeFails ("no store mapping for " ++ show addr)
