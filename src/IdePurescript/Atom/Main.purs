module IdePurescript.Atom.Main where

import Prelude
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Either (either)
import Data.Foreign(readBoolean)
import Data.Array (length)
import Data.Function.Eff (mkEffFn1)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, newRef)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Promise (Promise)
import Control.Promise as Promise
import Control.Monad (when)
import Control.Bind (join)
import DOM.Node.Types(Element)
import DOM (DOM)

import Node.FS (FS)
import Node.ChildProcess (CHILD_PROCESS)

import Atom.Atom (getAtom)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getText, getPath, getTextInRange, setTextInBufferRange)
import Atom.Range (mkRange)
import Atom.Point (Point, getRow, mkPoint)
import Atom.Config (CONFIG, getConfig)
import Atom.Project (PROJECT)
import Atom.Workspace (WORKSPACE, onDidChangeActivePaneItem, observeTextEditors, getActiveTextEditor)

import PscIde (NET)

import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.LinterBuild (lint, getProjectRoot)
import IdePurescript.Atom.Hooks.Linter (LinterInternal, LinterIndie, LINTER, register)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.PscIde (getPursuitModuleCompletion, getPursuitCompletion, loadDeps, getAvailableModules)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Modules (State, initialModulesState, getModulesForFile, getMainModule, getQualModule, getUnqualActiveModules, findImportInsertPos)
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Tooltips (registerTooltips)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.Psci as Psci
import IdePurescript.Atom.Pursuit as Pursuit
import IdePurescript.Atom.Imports (showAddImportsView)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.BuildStatus (getBuildStatus)

getSuggestions :: forall eff. State -> { editor :: TextEditor, bufferPosition :: Point }
  -> Eff (editor :: EDITOR, net :: NET | eff) (Promise (Array C.AtomSuggestion))
getSuggestions state ({editor, bufferPosition}) = do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- getTextInRange editor range
  let modules = getUnqualActiveModules state
      getQualifiedModule = (flip getQualModule) state
  Promise.fromAff $ C.getSuggestions { line, moduleInfo: { modules, getQualifiedModule }}

useEditor :: forall eff. (Ref State) -> TextEditor -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE | eff) Unit
useEditor modulesStateRef editor = do
  path <- getPath editor
  text <- getText editor
  let mainModule = getMainModule text
  case mainModule of
    Just m -> runAff logError ignoreError $ do
      loadDeps m
      state <- getModulesForFile path text
      liftEff $ writeRef modulesStateRef state
      pure unit
    Nothing -> pure unit

type MainEff =
  ( command :: COMMAND
  , note :: NOTIFY
  , project :: PROJECT
  , fs :: FS
  , ref :: REF
  , cp :: CHILD_PROCESS
  , console :: CONSOLE
  , config :: CONFIG
  , linter :: LINTER
  , editor :: EDITOR
  , net :: NET
  , workspace :: WORKSPACE
  , avar :: AVAR
  , err :: EXCEPTION
  , dom :: DOM
  )

main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  messagesRef <- newRef ([] :: Array AtomLintMessage)
  linterInternalRef <- newRef (Nothing :: Maybe LinterInternal)
  deactivateRef <- newRef (pure unit :: Eff MainEff Unit)
  buildStatusRef <- newRef (Nothing :: Maybe Element)

  let
    doLint :: Eff MainEff Unit
    doLint = do
      root <- getProjectRoot
      linterIndie <- readRef linterIndieRef
      statusElt <- readRef buildStatusRef
      case { root, linterIndie, statusElt } of
        { root: Just root', linterIndie: Just linterIndie', statusElt: Just statusElt' } -> runAff raiseError ignoreError $ do
          messages <- lint atom.config root' linterIndie' statusElt'
          liftEff $ maybe (pure unit) (writeRef messagesRef) messages
          editor <- liftEff $ getActiveTextEditor atom.workspace
          liftEff $ maybe (pure unit) (useEditor modulesState) editor
          pure unit
        _ -> pure unit

    quickFix :: Eff MainEff Unit
    quickFix = do
      messages <- readRef messagesRef
      editor <- getActiveTextEditor atom.workspace
      linter <- readRef linterInternalRef
      case { editor, linter, n: length messages } of
        { editor: Just e, linter: Just l, n } | n > 0 -> showQuickFixes e l messages
        _ -> pure unit

    pursuitSearch :: Eff MainEff Unit
    pursuitSearch = Pursuit.pursuitSearch (Promise.fromAff <<< getPursuitCompletion)

    addImport :: String -> Eff MainEff Unit
    addImport moduleName = do
      maybeEditor <- getActiveTextEditor atom.workspace
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          text <- getText editor
          let index = findImportInsertPos text
          let pt = (mkRange (mkPoint index 0) (mkPoint index 0))
          void $ setTextInBufferRange editor pt $ "import " ++ moduleName ++ "\n"

    pursuitSearchModule :: Eff MainEff Unit
    pursuitSearchModule = Pursuit.pursuitSearchModules
      (Promise.fromAff <<< getPursuitModuleCompletion)
      addImport

    addModuleImport :: Eff MainEff Unit
    addModuleImport = do
      showAddImportsView (Promise.fromAff getAvailableModules) addImport

    activate :: Eff MainEff Unit
    activate = do
      let cmd name action = addCommand atom.commands "atom-workspace" ("purescript:"++name) (const action)
      cmd "build" doLint
      cmd "show-quick-fixes" quickFix
      cmd "pursuit-search" pursuitSearch
      cmd "pursuit-search-modules" pursuitSearchModule
      cmd "add-module-import" addModuleImport

      observeTextEditors atom.workspace (\editor -> do -- TODO: Check if file is .purs
        useEditor modulesState editor
        onDidSave editor (\_ -> do
          buildOnSave <- getConfig atom.config "ide-purescript.buildOnSave"
          when (either (const false) id $ readBoolean buildOnSave) doLint -- TODO: Check if file is in project
        )
      )

      onDidChangeActivePaneItem atom.workspace (\item ->
        maybe (pure unit) (useEditor modulesState) (toEditor item)
      )

      registerTooltips modulesState
      runAff
        (\_ -> log "Error starting server")
        (\deact -> do
            writeRef deactivateRef deact
            editor <- getActiveTextEditor atom.workspace
            maybe (pure unit) (useEditor modulesState) editor)
        startServer
      Psci.init

    deactivate :: Eff MainEff Unit
    deactivate = join (readRef deactivateRef)

  pure
    { config
    , activate: mkEffFn1 \_ -> activate
    , deactivate: mkEffFn1 \_ -> deactivate
    , consumeLinterIndie: mkEffFn1 \registry -> do
        linterIndie <- register registry {name: "PureScript"}
        writeRef linterIndieRef $ Just linterIndie
    , consumeLinterInternal: mkEffFn1 \linter ->
        writeRef linterInternalRef $ Just linter
    , consumeStatusBar: mkEffFn1 \statusBar -> do
        item <- getBuildStatus
        writeRef buildStatusRef $ Just item
        addLeftTile statusBar { item, priority: -50 }
    , provideAutocomplete: \_ ->
        { selector: ".source.purescript"
        , disableForSelector: ".source.purescript .comment, .source.purescript .string"
        , inclusionPriority: 1
        , excludeLowerPriority: true
        , getSuggestions: mkEffFn1 $ \x -> do
            state <- readRef modulesState
            getSuggestions state x
        }
    }

raiseError :: forall eff. Error -> Eff (note :: NOTIFY | eff) Unit
raiseError e = do
  atom <- getAtom
  addError atom.notifications (show e)

ignoreError :: forall a eff. a -> Eff eff Unit
ignoreError _ = pure unit

logError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
logError e = error $ show e
