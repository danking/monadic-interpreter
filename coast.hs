module CoAst (pushin, CoExp(..)) where

import Ast

import Control.Comonad.Env

data CoExp w = CoAbs (w Id) (w Exp)
             | CoVar (w Id)
             | CoApp (w Exp) (w Exp)
             | CoBOp BinOp (w Exp) (w Exp)
             | CoIf (w Exp) (w Exp) (w Exp)

pushin :: Comonad w => w Exp -> CoExp w
pushin e = case (extract e) of
  Abs x b     -> CoAbs (w x e) (w b e)
  Var x       -> CoVar (w x e)
  App e₁ e₂   -> CoApp (w e₁ e) (w e₂ e)
  BOp o e₁ e₂ -> CoBOp o (w e₁ e) (w e₂ e)
  If p c a    -> CoIf (w p e) (w c e) (w a e)
  -- NB: there seems to be some type inference issue if you try to close over e
  where w v e = extend (const v) e
