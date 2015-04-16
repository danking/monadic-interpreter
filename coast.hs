{-# LANGUAGE GADTs #-}
module CoAst (pushin, CoExp(..)) where

import Ast

import Control.Comonad.Env

data CoExp w where
  CoAbs :: (w Id) -> (w Exp) -> CoExp w
  CoVar :: (w Id) -> CoExp w
  CoApp :: (w Exp) -> (w Exp) -> CoExp w
  CoIf  :: (w Exp) -> (w Exp) -> (w Exp) -> CoExp w

pushin :: Comonad w => w Exp -> CoExp w
pushin e = case (extract e) of
  Abs x b   -> CoAbs (w x e) (w b e)
  Var x     -> CoVar (w x e)
  App e₁ e₂ -> CoApp (w e₁ e) (w e₂ e)
  If p c a  -> CoIf (w p e) (w c e) (w a e)
  -- NB: there seems to be some type inference issue if you try to close over e
  where w v e = extend (const v) e
