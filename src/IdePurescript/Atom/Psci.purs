module IdePurescript.Atom.Psci where

import Prelude (Unit)
import Control.Monad.Eff (Eff)

foreign import init :: forall eff. Eff eff Unit
