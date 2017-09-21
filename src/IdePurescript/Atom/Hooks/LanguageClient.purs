module IdePurescript.Atom.Hooks.LanguageClient where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

foreign import data LanguageClient :: Type
foreign import makeLanguageClient :: forall eff. Eff (exception :: EXCEPTION | eff) LanguageClient
