module IdePurescript.Atom.Main where

import Prelude

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR)
import Atom.Grammar (GRAMMAR)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Pane (PANE)
import Atom.Project (PROJECT)
import Atom.Workspace (WORKSPACE)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2, runEffFn4, runEffFn6, EffFn1)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, F, readNumber, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Int (floor)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import IdePurescript.Atom.Assist (addClause, caseSplit, fixTypo, fixTypoWithRange)
import IdePurescript.Atom.BuildStatus (getBuildStatus, updateBuildStatus, BuildStatus(..))
import IdePurescript.Atom.Config (config, translateConfig)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand, makeLanguageClient, onCustom)
import IdePurescript.Atom.Hooks.Linter (LINTER)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Imports (addExplicitImport, addModuleImport)
import IdePurescript.Atom.Psci as Psci
import IdePurescript.Atom.Search (localSearch, pursuitSearch, pursuitSearchModule)
import IdePurescript.Atom.Util (launchAffAndRaise)
import Network.HTTP.Affjax (AJAX)
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
  , ajax :: AJAX
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

  let cmd s f = Tuple s (mkEffFn1 f)
      fwdCmd name name' = cmd name \conn -> launchAffAndRaise $
        executeCommand conn { command: "purescript."<>name', arguments: [] }

      commands :: Array (Tuple String (EffFn1 MainEff LanguageClientConnection Unit))
      commands =
        [ cmd "add-module-import" addModuleImport
        , cmd "add-explicit-import" addExplicitImport
        , cmd "case-split" caseSplit
        , cmd "add-clause" addClause

        , cmd "fixTypo" fixTypo
        , cmd "search" localSearch
        , cmd "pursuit-search" \_ -> pursuitSearch
        , cmd "pursuit-search-modules" pursuitSearchModule

        , fwdCmd "build" "build"
        , fwdCmd "restart-psc-ide" "restartPscIde"
        , fwdCmd "start-psc-ide" "startPscIde"
        , fwdCmd "stop-psc-ide" "stopPscIde"
        ]

  languageClient <- runEffFn6 makeLanguageClient {
        config
      , consumeStatusBar: mkEffFn1 \statusBar -> addLeftTile statusBar { item: buildStatusElt, priority: -50 }
      } translateConfig (mkEffFn2 fixTypoWithRange)
      (mkEffFn1 $ \conn -> do
        activate
        onCustom conn "textDocument/diagnosticsBegin" $ \_ -> updateBuildStatus buildStatusElt Building
        onCustom conn "textDocument/diagnosticsEnd" $ \_ -> updateBuildStatus buildStatusElt NotBuilding

        onCustom conn "window/logMessage" $ either (pure $ pure unit) id <<< runExcept <<<
          \x -> do
            -- TODO conditionally log based on config
            level <- x ! "type" >>= readNumber
            message <- x ! "message" >>= readString
            pure $ log $ show (floor level) <> ": " <> message)
      (pure unit)
      (StrMap.fromFoldable commands)

  pure $ toForeign $ languageClient

raiseError :: forall eff. Error -> Eff (note :: NOTIFY | eff) Unit
raiseError e = do
  atom <- getAtom
  addError atom.notifications (show e)

ignoreError :: forall a eff. a -> Eff eff Unit
ignoreError _ = pure unit

logError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
logError e = error $ show e
