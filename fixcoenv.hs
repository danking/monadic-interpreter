module FixCoEnv (asks) where

import Control.Comonad.Env hiding (asks)

-- Coenv doesn't have the right signature for `asks`, this is what it should be
asks :: ComonadEnv e w => w (e -> e') -> e'
asks f = (extract f) (ask f)
