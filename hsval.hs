module HSVal ( ValLike
             , inj
             , prj
             ) where

import Coercions
import EvalMonad
import Val

class ValLike a where
  inj :: a -> Val
  prj :: Val -> EvalMonad a

instance ValLike Integer where
  inj v = I v
  prj = coerceInt
instance ValLike Bool where
  inj v = B v
  prj = coerceBool
