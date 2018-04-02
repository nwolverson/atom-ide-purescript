module IdePurescript.Atom.Hooks.LanguageClient (LanguageClient, LanguageClientConnection, ExecuteCommandParams, makeLanguageClient, executeCommand, onCustom) where

import Prelude

import Atom.Range (Range)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, EffFn5,EffFn6, mkEffFn1, runEffFn2, runEffFn3)
import Control.Promise (Promise, toAffE)
import Data.Foreign (Foreign)
import Data.StrMap (StrMap)

foreign import data LanguageClient :: Type
foreign import data LanguageClientConnection :: Type

foreign import makeLanguageClient :: forall eff r. EffFn6 (exception :: EXCEPTION | eff) {|r}
  (Foreign -> Foreign)
  (EffFn2 (exception :: EXCEPTION | eff) LanguageClientConnection Range Unit)
  (EffFn1 (exception :: EXCEPTION | eff) LanguageClientConnection Unit)
  (Eff (exception :: EXCEPTION | eff) Unit)
  (StrMap (EffFn1 (exception :: EXCEPTION | eff) LanguageClientConnection Unit))
  LanguageClient

type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

foreign import executeCommandImpl :: forall eff. EffFn2 (exception :: EXCEPTION | eff) LanguageClientConnection ExecuteCommandParams (Promise Foreign)

executeCommand :: forall eff. LanguageClientConnection -> ExecuteCommandParams -> Aff (exception :: EXCEPTION | eff) Foreign
executeCommand conn params = toAffE $ runEffFn2 executeCommandImpl conn params

foreign import onCustomImpl :: forall eff. EffFn3 (exception :: EXCEPTION | eff) LanguageClientConnection String (EffFn1 (exception :: EXCEPTION | eff) Foreign Unit) Unit

onCustom :: forall eff. LanguageClientConnection -> String -> (Foreign -> Eff (exception :: EXCEPTION | eff) Unit) -> Eff (exception :: EXCEPTION | eff) Unit
onCustom conn name cb = runEffFn3 onCustomImpl conn name (mkEffFn1 cb)
