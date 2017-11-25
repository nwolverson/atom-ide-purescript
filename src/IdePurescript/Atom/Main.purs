module IdePurescript.Atom.Main where

import Prelude

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand, dispatchRoot)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getPath, getText, getTextInRange)
import Atom.Grammar (GRAMMAR)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Pane (PANE)
import Atom.Project (PROJECT)
import Atom.Workspace (WORKSPACE, onDidChangeActivePaneItem, observeTextEditors, getActiveTextEditor)
import Control.Monad.Aff.AVar (putVar, takeVar, makeVar', AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, modifyRef, newRef)
import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2, runEffFn1, runEffFn3, runEffFn2)
import DOM (DOM)
import Data.Foreign (Foreign, readBoolean, toForeign)
import IdePurescript.Atom.Config (autoCompleteAllModules, autoCompleteGrouped, autoCompleteLimit, autoCompletePreferredModules, config, translateConfig)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LINTER, LinterIndie)
import IdePurescript.Atom.Hooks.LanguageClient (makeLanguageClient, executeCommand, onCustom)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.BuildStatus (getBuildStatus, updateBuildStatus, BuildStatus(..))
import IdePurescript.Atom.Psci as Psci
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

type MainEff =
  ( command :: COMMAND
  , note :: NOTIFY
  , project :: PROJECT
  , fs :: FS
  , buffer :: BUFFER
  , ref :: REF
  , cp :: CHILD_PROCESS
  , process :: PROCESS
  , console :: CONSOLE
  , config :: CONFIG
  , linter :: LINTER
  , editor :: EDITOR
  , net :: NET
  , workspace :: WORKSPACE
  , pane :: PANE
  , avar :: AVAR
  , exception :: EXCEPTION
  , dom :: DOM
  , grammar :: GRAMMAR
  , random :: RANDOM
  )

main :: Eff MainEff Foreign
main = do
  log "PureScript: Starting!"
  atom <- getAtom

  let activate :: Eff MainEff Unit
      activate = do
        Psci.registerCommands
        installDependencies
        Psci.activate

  buildStatusElt <- getBuildStatus

  languageClient <- runEffFn3 makeLanguageClient {
        config
      , consumeStatusBar: mkEffFn1 \statusBar -> addLeftTile statusBar { item: buildStatusElt, priority: -50 }
      } translateConfig $ mkEffFn1 $ \conn -> do
    activate
    let fwdCmd name name' = addCommand atom.commands "atom-workspace" ("ide-purescript:"<>name)
                        (const $ runEffFn2 executeCommand conn { command: "purescript."<>name', arguments: [] })
        fwdCmd' name = fwdCmd name name


              -- "ide-purescript:add-module-import",
              -- "ide-purescript:add-explicit-import",
              -- "ide-purescript:case-split",
              -- "ide-purescript:add-clause",
              -- "ide-purescript:fix-typo",

              -- "ide-purescript:search",
              -- "ide-purescript:pursuit-search",
              -- "ide-purescript:pursuit-search-modules",
              -- "ide-purescript:psci-open",
              -- "ide-purescript:psci-send-line",
              -- "ide-purescript:psci-send-selection"

    fwdCmd' "build"
    fwdCmd "restart-psc-ide" "restartPscIde"
    fwdCmd "start-psc-ide" "startPscIde"
    fwdCmd "stop-psc-ide" "stopPscIde"


    runEffFn3 onCustom conn "textDocument/diagnosticsBegin" $ mkEffFn1 \_ -> updateBuildStatus buildStatusElt Building
    runEffFn3 onCustom conn "textDocument/diagnosticsEnd" $ mkEffFn1 \_ -> updateBuildStatus buildStatusElt NotBuilding


  pure $ toForeign $ languageClient

raiseError :: forall eff. Error -> Eff (note :: NOTIFY | eff) Unit
raiseError e = do
  atom <- getAtom
  addError atom.notifications (show e)

ignoreError :: forall a eff. a -> Eff eff Unit
ignoreError _ = pure unit

logError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
logError e = error $ show e
