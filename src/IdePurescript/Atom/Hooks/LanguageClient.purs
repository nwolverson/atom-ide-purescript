module IdePurescript.Atom.Hooks.LanguageClient where

import Prelude
import Data.Foreign (Foreign)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3)
import Control.Monad.Eff.Exception (EXCEPTION)

foreign import data LanguageClient :: Type
foreign import data LanguageClientConnection :: Type

foreign import makeLanguageClient :: forall eff. EffFn3 (exception :: EXCEPTION | eff) Foreign (Foreign -> Foreign) (EffFn1 (exception :: EXCEPTION | eff) LanguageClientConnection Unit) LanguageClient

type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

foreign import executeCommand :: forall eff. EffFn2 (exception :: EXCEPTION | eff) LanguageClientConnection ExecuteCommandParams Unit
