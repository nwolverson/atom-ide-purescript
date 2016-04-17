module IdePurescript.Atom.Search where

import Prelude
import Atom.Editor (EDITOR)
import Atom.NotificationManager (NOTIFY)
import Atom.Workspace (WORKSPACE)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import IdePurescript.Atom.Imports (addImport)
import IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic)
import IdePurescript.Modules (State, getQualModule)
import IdePurescript.PscIde (getCompletion, getLoadedModules, getPursuitModuleCompletion, getPursuitCompletion)
import Node.FS (FS)
import PscIde (NET)

type LocalEff eff = (dom :: DOM, console :: CONSOLE, net :: NET | eff)
type PursuitEff eff = (dom :: DOM
    , workspace :: WORKSPACE
    , ref :: REF
    , note :: NOTIFY
    , net :: NET
    , editor :: EDITOR
    , fs :: FS
    | eff)

pursuitSearch :: forall eff. Eff (LocalEff eff) Unit
pursuitSearch = selectListViewDynamic view (\x -> log x.identifier) Nothing (const "") getPursuitCompletion 1000
  where
  view {identifier, "type": ty, "module": mod, package} =
     "<li class='two-lines'>"
     ++ "<div class='primary-line'>" ++ identifier ++ ": <span class='text-info'>" ++ ty ++ "</span></div>"
     ++ "<div class='secondary-line'>" ++ mod ++ " (" ++ package ++ ")</div>"
     ++ "</li>"

pursuitSearchModule :: forall eff. Ref State -> Eff (PursuitEff eff) Unit
pursuitSearchModule modulesState = selectListViewDynamic view importDialog (Just "module") id getPursuitModuleCompletion 1000
  where
  view {"module": mod, package} =
     "<li class='two-lines'>"
     ++ "<div class='primary-line'>" ++ mod ++ "</span></div>"
     ++ "<div class='secondary-line'>" ++ package ++ "</div>"
     ++ "</li>"
  importDialog :: forall a. {"module" :: String | a} -> Eff (PursuitEff eff) Unit
  importDialog {"module": mod} = selectListViewStatic textView (doImport mod) Nothing ["Import module", "Cancel"]
    where
    textView x = "<li>" ++ x ++ "</li>"
    doImport mod x = when (x == "Import module") $ addImport modulesState mod

localSearch ::forall eff. Ref State -> Eff (LocalEff (ref :: REF | eff)) Unit
localSearch modulesState = selectListViewDynamic view (\x -> log x.identifier) Nothing (const "") search 50
  where
  search text = do
    state <- liftEff $ readRef modulesState
    modules <- getLoadedModules
    let getQualifiedModule = (flip getQualModule) state
    getCompletion text "" false modules getQualifiedModule

  view {identifier, "type": ty, "module": mod} =
     "<li class='two-lines'>"
     ++ "<div class='primary-line'>" ++ identifier ++ ": <span class='text-info'>" ++ ty ++ "</span></div>"
     ++ "<div class='secondary-line'>" ++ mod ++ "</div>"
     ++ "</li>"
