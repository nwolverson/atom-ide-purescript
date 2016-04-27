module IdePurescript.Atom.Main where

import Prelude
import Control.Promise as Promise
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Psci as Psci
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getPath, getText, getTextInRange)
import Atom.Grammar (GRAMMAR)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Pane (PANE)
import Atom.Point (Point, getRow, mkPoint)
import Atom.Project (PROJECT)
import Atom.Range (mkRange)
import Atom.Workspace (WORKSPACE, onDidChangeActivePaneItem, observeTextEditors, getActiveTextEditor)
import Control.Bind (join)
import Control.Monad (when)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, newRef)
import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Either (either, Either(..))
import Data.Foreign (Foreign, readBoolean, toForeign)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (contains)
import IdePurescript.Atom.Assist (fixTypo, addClause, caseSplit)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Config (getPscIdePort, config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LinterInternal, LinterIndie, LINTER, register)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Imports (addSuggestionImport, addExplicitImportCmd, addModuleImportCmd)
import IdePurescript.Atom.LinterBuild (lint, getProjectRoot)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Atom.Search (localSearch, pursuitSearchModule, pursuitSearch)
import IdePurescript.Atom.Tooltips (registerTooltips)
import IdePurescript.Modules (State, getQualModule, initialModulesState, getModulesForFile, getMainModule, getUnqualActiveModules)
import IdePurescript.PscIde (loadDeps, getLoadedModules)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import PscIde (NET)

getSuggestions :: forall eff. State -> { editor :: TextEditor, bufferPosition :: Point, activatedManually :: Boolean }
  -> Eff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Promise (Array C.AtomSuggestion))
getSuggestions state ({editor, bufferPosition, activatedManually}) = Promise.fromAff $ flip catchError (raiseError' []) $ do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- liftEff'' $ getTextInRange editor range
  atom <- liftEff'' getAtom
  port <- liftEff'' getPscIdePort
  configRaw <- liftEff'' $ getConfig atom.config "ide-purescript.autocomplete.allModules"
  let autoCompleteAllModules = either (const false) id $ readBoolean configRaw
  modules <- if activatedManually || autoCompleteAllModules then getLoadedModules port else pure $ getUnqualActiveModules state Nothing
  let getQualifiedModule = (flip getQualModule) state
  C.getSuggestions port { line, moduleInfo: { modules, getQualifiedModule }}
  where
  raiseError' :: (Array C.AtomSuggestion) -> Error -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Array C.AtomSuggestion)
  raiseError' x e = do
    liftEff $ raiseError e
    pure x
  liftEff'' :: forall a. Eff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) a -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) a
  liftEff'' = liftEff

useEditor :: forall eff. (Ref State) -> TextEditor -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE, config :: CONFIG | eff) Unit
useEditor modulesStateRef editor = do
  path <- getPath editor
  text <- getText editor
  port <- getPscIdePort
  let mainModule = getMainModule text
  case path, mainModule of
    Just path', Just m -> runAff logError ignoreError $ do
      -- We load all deps initially, but only post 0.8.4, and maybe something resets psc-ide state
      loadDeps port m
      state <- getModulesForFile port path' text
      liftEff $ writeRef modulesStateRef state
      pure unit
    _, _ -> pure unit

type MainEff =
  ( command :: COMMAND
  , note :: NOTIFY
  , project :: PROJECT
  , fs :: FS
  , buffer :: BUFFER
  , ref :: REF
  , cp :: CHILD_PROCESS
  , console :: CONSOLE
  , config :: CONFIG
  , linter :: LINTER
  , editor :: EDITOR
  , net :: NET
  , workspace :: WORKSPACE
  , pane :: PANE
  , avar :: AVAR
  , err :: EXCEPTION
  , dom :: DOM
  , grammar :: GRAMMAR
  )

main :: Eff MainEff Foreign
main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  messagesRef <- newRef ([] :: Array AtomLintMessage)
  linterInternalRef <- newRef (Nothing :: Maybe LinterInternal)
  deactivateRef <- newRef (pure unit :: Eff MainEff Unit)
  buildStatusRef <- newRef (Nothing :: Maybe Element)

  let
    doLint :: (Maybe String) -> Eff MainEff Unit
    doLint file = do
      root <- getProjectRoot
      linterIndie <- readRef linterIndieRef
      statusElt <- readRef buildStatusRef
      port <- getPscIdePort
      case { root, linterIndie, statusElt } of
        { root: Just root', linterIndie: Just linterIndie', statusElt: Just statusElt' } -> runAff raiseError ignoreError $ do
          messages <- lint file atom.config root' linterIndie' statusElt'
          liftEff $ maybe (pure unit) (writeRef messagesRef) messages
          P.load port [] []
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
        { editor: Just e, linter: Just l, n } | n > 0 -> showQuickFixes modulesState e l messages
        _ -> pure unit

    restartPscIdeServer :: Eff MainEff Unit
    restartPscIdeServer = do
      join $ readRef deactivateRef
      startPscIdeServer

    startPscIdeServer :: Eff MainEff Unit
    startPscIdeServer =
      runAff
        (\_ -> log "Error starting server")
        ignoreError
        do
          port <- liftEff $ getPscIdePort
          deact <- startServer
          liftEff $ writeRef deactivateRef deact
          P.load port [] []
          liftEff $ do
            editor <- getActiveTextEditor atom.workspace
            maybe (pure unit) (useEditor modulesState) editor

    activate :: Eff MainEff Unit
    activate = do
      port <- getPscIdePort
      let cmd name action = addCommand atom.commands "atom-workspace" ("purescript:"++name) (const action)
      cmd "build" $ doLint Nothing
      cmd "show-quick-fixes" quickFix
      cmd "pursuit-search" $ pursuitSearch port
      cmd "pursuit-search-modules" $ pursuitSearchModule port modulesState
      cmd "add-module-import" $ addModuleImportCmd port modulesState
      cmd "add-explicit-import" $ addExplicitImportCmd port modulesState
      cmd "search" $ localSearch port modulesState
      cmd "case-split" caseSplit
      cmd "add-clause" addClause
      cmd "fix-typo" $ fixTypo modulesState
      cmd "restart-psc-ide" $ restartPscIdeServer

      installDependencies

      observeTextEditors atom.workspace (\editor -> do
        path <- getPath editor
        case path of
          Just path' | contains ".purs" path' -> do
            useEditor modulesState editor
            onDidSave editor (\_ -> do
              buildOnSave <- getConfig atom.config "ide-purescript.buildOnSave"
              when (either (const false) id $ readBoolean buildOnSave) (doLint path)-- TODO: Check if file is in project
            )
          _ -> pure unit
      )

      onDidChangeActivePaneItem atom.workspace (\item ->
        maybe (pure unit) (useEditor modulesState) (toEditor item)
      )

      registerTooltips port modulesState
      startPscIdeServer

      Psci.activate

    deactivate :: Eff MainEff Unit
    deactivate = join (readRef deactivateRef)

  pure $ toForeign
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
        , onDidInsertSuggestion: mkEffFn1 \x -> do
            shouldAddImport <- getConfig atom.config "ide-purescript.autocomplete.addImport"
            port <- getPscIdePort
            when (readBoolean shouldAddImport == Right true)
              (runAff raiseError ignoreError $ addSuggestionImport port modulesState x)
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
