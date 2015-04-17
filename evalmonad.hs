module EvalMonad ( address
                 , bindLocal
                 , injectProg
                 , lookup
                 , pushKon
                 , popKon
                 , runEvalMonad
                 , runEvalMonad'
                 , store
                 , EvalMonad
                 , EvalComonad
                 , Failure
                 , Store
                 ) where

import Ast
import KonValEnv

import FixCoEnv (asks)

import qualified Control.Comonad.Env as Co
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Identity
import qualified Data.Map as M
import Prelude hiding (lookup)

type Store = M.Map Addr Val
type EvalComonad = Co.Env (Kon, Env)
type Failure = String
type EvalMonad = StateT (Addr, Store) (Either Failure)

injectProg :: Exp -> EvalComonad Exp
injectProg = Co.env (halt, emptyenv)

emptystate :: (Addr, Store)
emptystate = (0, M.empty)

runEvalMonad :: EvalMonad a -> Either Failure a
runEvalMonad ma = fst `fmap` runStateT ma emptystate

runEvalMonad' :: EvalMonad a -> Either Failure (a, (Addr, Store))
runEvalMonad' ma = runStateT ma emptystate


askK :: EvalComonad a -> Kon
askK = fst . Co.ask

asksK :: EvalComonad (Kon -> a) -> a
asksK a = asks $ fmap (. fst) a

askE :: EvalComonad a -> Env
askE = snd . Co.ask

asksE :: EvalComonad (Env -> a) -> a
asksE a = asks $ fmap (. snd) a

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

infix 3 ×
(×) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(×) f g (a,b) = (f a, g b)

pushKon :: Frame -> EvalComonad a -> EvalComonad a
pushKon k e =
  Co.local push e
  where push = (k :) × id

popKon :: EvalComonad a -> EvalComonad Frame
popKon e =
  Co.local pop (Co.extend (head . askK) e)
  where pop = tail × id

bindLocal :: Id -> Addr -> EvalComonad a -> EvalComonad a
bindLocal x a b =
  Co.local bind b
  where bind = id × M.insert x a

store :: Val -> EvalMonad Addr
store v = do
  a <- getA
  s <- getS
  putS $ M.insert a v s
  putA (a + 1)
  return a

address :: EvalComonad Id -> EvalMonad Addr
address id = kill $ asksE $ M.lookup `fmap` id
  where kill = maybeFails ("no environment mapping for " ++ show (Co.extract id))

lookup :: Addr -> EvalMonad Val
lookup addr = (M.lookup addr) `fmap` getS >>= kill
  where kill = maybeFails ("no store mapping for " ++ show addr)
