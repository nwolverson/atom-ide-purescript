module IdePurescript.Atom.Hooks.LanguageClient where

import Prelude
import Data.Foreign (Foreign)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2)
import Control.Monad.Eff.Exception (EXCEPTION)

foreign import data LanguageClient :: Type
foreign import data LanguageClientConnection :: Type

foreign import makeLanguageClient :: forall eff. EffFn1 (exception :: EXCEPTION | eff) (EffFn1 (exception :: EXCEPTION | eff) LanguageClientConnection Unit) LanguageClient

type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

foreign import executeCommand :: forall eff. EffFn2 (exception :: EXCEPTION | eff) LanguageClientConnection ExecuteCommandParams Unit
