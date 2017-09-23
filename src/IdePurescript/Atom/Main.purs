module IdePurescript.Atom.Main where

import Prelude

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand, dispatchRoot)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getPath, getText, getTextInRange)
import Atom.Grammar (GRAMMAR)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Pane (PANE)
import Atom.Point (Point, getRow, mkPoint)
import Atom.Project (PROJECT)
import Atom.Range (mkRange)
import Atom.Workspace (WORKSPACE, onDidChangeActivePaneItem, observeTextEditors, getActiveTextEditor)
import Control.Monad.Aff (attempt, Aff, runAff, delay)
import Control.Monad.Aff.AVar (putVar, takeVar, makeVar', AVAR)
import Control.Monad.Aff.Internal (AVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, modifyRef, newRef)
import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2, runEffFn1, runEffFn2)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Promise (Promise)
import Control.Promise as Promise
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (sequence_, traverse_)
import Data.Foreign (Foreign, readBoolean, toForeign)
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Nullable (toNullable)

import Data.StrMap (lookup, empty)
import Data.StrMap as StrMap
import Data.String (Pattern(Pattern), contains)
import Data.Time.Duration (Milliseconds(..))

import IdePurescript.Atom.Assist (gotoDefHyper, fixTypo, addClause, caseSplit, gotoDef)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Config (autoCompleteAllModules, autoCompleteGrouped, autoCompleteLimit, autoCompletePreferredModules, config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LINTER, LinterIndie)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Hooks.LanguageClient (makeLanguageClient, executeCommand)
import IdePurescript.Atom.Imports (addExplicitImportCmd, addModuleImportCmd)
import IdePurescript.Atom.LinterBuild (lint)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.Psci (registerCommands)
import IdePurescript.Atom.Psci as Psci
import IdePurescript.Atom.Search (localSearch, pursuitSearchModule, pursuitSearch)
import IdePurescript.Atom.Tooltips (registerTooltips, showTooltipAtCursor)
import IdePurescript.Modules (State, getAllActiveModules, getModulesForFile, getQualModule, getUnqualActiveModules, initialModulesState)
import IdePurescript.PscIde (getLoadedModules)

import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)
import PscIde as P
import PscIde.Project (getRoot)

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
        registerCommands
        installDependencies
        Psci.activate

  languageClient <- runEffFn1 makeLanguageClient $ mkEffFn1 $ \conn -> do
    let fwdCmd name name' = addCommand atom.commands "atom-workspace" ("ide-purescript:"<>name)
                        (const $ runEffFn2 executeCommand conn { command: "purescript."<>name', arguments: [] })
        fwdCmd' name = fwdCmd name name

              -- "ide-purescript:build",

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

  buildStatusRef <- newRef (Nothing :: Maybe Element)

  pure $ toForeign $ languageClient

raiseError :: forall eff. Error -> Eff (note :: NOTIFY | eff) Unit
raiseError e = do
  atom <- getAtom
  addError atom.notifications (show e)

ignoreError :: forall a eff. a -> Eff eff Unit
ignoreError _ = pure unit

logError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
logError e = error $ show e
