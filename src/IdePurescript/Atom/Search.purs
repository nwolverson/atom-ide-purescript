module IdePurescript.Atom.Search where

import Prelude
import PscIde.Command as C
import Atom.Editor (EDITOR)
import Atom.NotificationManager (NOTIFY)
import Atom.Workspace (WORKSPACE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import IdePurescript.Atom.Imports (addImport)
import IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic)
import IdePurescript.Modules (State, getQualModule)
import IdePurescript.PscIde (getCompletion', getLoadedModules, getPursuitModuleCompletion, getPursuitCompletion)
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

pursuitSearch :: forall eff. Int -> Eff (LocalEff eff) Unit
pursuitSearch port = selectListViewDynamic view (\x -> log x.identifier) Nothing (const "") (getPursuitCompletion port) 1000
  where
  view {identifier, "type": ty, "module": mod, package} =
     "<li class='two-lines'>"
     <> "<div class='primary-line'>" <> identifier <> ": <span class='text-info'>" <> ty <> "</span></div>"
     <> "<div class='secondary-line'>" <> mod <> " (" <> package <> ")</div>"
     <> "</li>"

pursuitSearchModule :: forall eff. Int -> Ref State -> Eff (PursuitEff eff) Unit
pursuitSearchModule port modulesState = selectListViewDynamic view importDialog (Just "module") id (getPursuitModuleCompletion port) 1000
  where
  view {"module": mod, package} =
     "<li class='two-lines'>"
     <> "<div class='primary-line'>" <> mod <> "</span></div>"
     <> "<div class='secondary-line'>" <> package <> "</div>"
     <> "</li>"
  importDialog :: forall a. {"module" :: String | a} -> Eff (PursuitEff eff) Unit
  importDialog {"module": mod} = selectListViewStatic textView (doImport mod) Nothing ["Import module", "Cancel"]
    where
    textView x = "<li>" <> x <> "</li>"
    doImport mod x = when (x == "Import module") $ addImport port modulesState mod

localSearch ::forall eff. Int -> Ref State -> Eff (LocalEff (ref :: REF | eff)) Unit
localSearch port modulesState = selectListViewDynamic view (\(C.TypeInfo { identifier }) -> log identifier) Nothing (const "") search 50
  where
  search text = do
    state <- liftEff $ readRef modulesState
    modules <- getLoadedModules port
    let getQualifiedModule = (flip getQualModule) state
    getCompletion' (Just $ C.Flex text) [] port state.main "" false modules getQualifiedModule

  view (C.TypeInfo {identifier, type', module'}) =
     "<li class='two-lines'>"
     <> "<div class='primary-line'>" <> identifier <> ": <span class='text-info'>" <> type' <> "</span></div>"
     <> "<div class='secondary-line'>" <> module' <> "</div>"
     <> "</li>"
