module IdePurescript.Atom.Main where

import Prelude (Unit, unit, pure, bind, ($), id, const, (>), flip)
import Data.Maybe(maybe,Maybe(..))
import Data.Either (either)
import Data.Foreign(readBoolean)
import Data.Array(length)
import Data.Function.Eff (mkEffFn1)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, newRef)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise, fromAff)
import Control.Monad (when)

import Node.FS (FS)
import Node.ChildProcess (CHILD_PROCESS)

import Atom.Atom (getAtom)
import Atom.NotificationManager (NOTIFY, addInfo)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getText, getPath, getTextInRange)
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
import IdePurescript.PscIde (loadDepsA)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Modules (State, initialModulesState, getModulesForFile, getMainModule, getQualModule, getUnqualActiveModules)
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Tooltips (registerTooltips)

getSuggestions :: forall eff. State -> { editor :: TextEditor, bufferPosition :: Point }
  -> Eff (editor :: EDITOR, net :: NET | eff) (Promise (Array C.AtomSuggestion))
getSuggestions state ({editor, bufferPosition}) = do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- getTextInRange editor range
  let modules = getUnqualActiveModules state
      getQualifiedModule = (flip getQualModule) state
  fromAff $ C.getSuggestions { line, moduleInfo: { modules, getQualifiedModule }}

useEditor :: forall eff. (Ref State) -> TextEditor -> Eff (editor ::EDITOR, net :: NET, ref :: REF | eff) Unit
useEditor modulesStateRef editor = do
  path <- getPath editor
  text <- getText editor
  let mainModule = getMainModule text
  case mainModule of
    Just m -> runAff ignoreError ignoreError $ do
      loadDepsA m
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
  )

main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  messagesRef <- newRef ([] :: Array AtomLintMessage)
  linterInternalRef <- newRef (Nothing :: Maybe LinterInternal)

  let
    doLint :: Eff MainEff Unit
    doLint = do
      addInfo atom.notifications "Building PureScript"
      root <- getProjectRoot
      linterIndie <- readRef linterIndieRef
      case { root, linterIndie } of
        { root: Just root', linterIndie: Just linterIndie' } -> runAff ignoreError ignoreError $ do
          messages <- lint atom.config root' linterIndie'
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

  addCommand atom.commands "atom-workspace" "purescript:build" $ const doLint
  addCommand atom.commands "atom-workspace" "purescript:show-quick-fixes" $ const quickFix

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

  -- TODO: activate psc-ide-server
  -- TODO: commands:
  -- atom.commands.add("atom-workspace", "purescript:pursuit-search", @pursuit.search)
  -- atom.commands.add("atom-workspace", "purescript:pursuit-search-modules", @pursuit.searchModule)
  -- atom.commands.add("atom-workspace", "purescript:add-module-import", =>

  pure
    { config
    , consumeLinterIndie: mkEffFn1 \registry -> do
        linterIndie <- register registry {name: "PureScript"}
        writeRef linterIndieRef $ Just linterIndie
    , consumeLinterInternal: mkEffFn1 \linter ->
        writeRef linterInternalRef $ Just linter
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

ignoreError :: forall a eff. a -> Eff eff Unit
ignoreError _ = pure unit
