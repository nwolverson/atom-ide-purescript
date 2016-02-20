module IdePurescript.Atom.Psci where

import Prelude
import Control.Monad.Eff

foreign import init :: forall eff. Eff eff Unit
