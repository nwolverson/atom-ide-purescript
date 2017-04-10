module IdePurescript.Atom.Hooks.Dependencies where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

foreign import installDependencies :: forall eff. Eff (exception :: EXCEPTION | eff) Unit
