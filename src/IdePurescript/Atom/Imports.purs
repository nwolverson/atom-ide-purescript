module IdePurescript.Atom.Imports (showAddImportsView) where

import Prelude (Unit)
import Data.Function.Eff (EffFn1, EffFn2, mkEffFn1, runEffFn2)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise)

foreign import showAddImportsViewImpl :: forall eff. EffFn2 eff (Eff eff (Promise (Array String))) (EffFn1 eff String Unit) Unit

showAddImportsView :: forall eff. Eff eff (Promise (Array String)) -> (String -> Eff eff Unit) -> Eff eff Unit
showAddImportsView getImports addImport = runEffFn2 showAddImportsViewImpl getImports (mkEffFn1 addImport)
