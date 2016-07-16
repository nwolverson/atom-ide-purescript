module IdePurescript.Atom.Main where

import Prelude
import Control.Promise as Promise
import Data.StrMap as StrMap
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
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (modifyRef, REF, Ref, readRef, writeRef, newRef)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Promise (Promise)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Either (either)
import Data.Foreign (Foreign, readBoolean, toForeign)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.StrMap (lookup, empty)
import Data.String (contains)
import Data.Traversable (sequence)
import IdePurescript.Atom.Assist (fixTypo, addClause, caseSplit, gotoDef)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LinterInternal, LinterIndie, LINTER, register)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Imports (addSuggestionImport, addExplicitImportCmd, addModuleImportCmd)
import IdePurescript.Atom.LinterBuild (getRoot, lint)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.Psci (registerCommands)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Atom.Search (localSearch, pursuitSearchModule, pursuitSearch)
import IdePurescript.Atom.Tooltips (registerTooltips)
import IdePurescript.Modules (State, getQualModule, initialModulesState, getModulesForFile, getMainModule, getUnqualActiveModules)
import IdePurescript.PscIde (loadDeps, getLoadedModules)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

getSuggestions :: forall eff. Int -> State -> { editor :: TextEditor, bufferPosition :: Point, activatedManually :: Boolean }
  -> Eff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Promise (Array C.AtomSuggestion))
getSuggestions port state ({editor, bufferPosition, activatedManually}) = Promise.fromAff $ flip catchError (raiseError' []) $ do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- liftEff'' $ getTextInRange editor range
  atom <- liftEff'' getAtom
  configRaw <- liftEff'' $ getConfig atom.config "ide-purescript.autocomplete.allModules"
  let autoCompleteAllModules = either (const false) id $ readBoolean configRaw
  modules <- if activatedManually || autoCompleteAllModules then getLoadedModules port else pure $ getUnqualActiveModules state Nothing
  let getQualifiedModule = (flip getQualModule) state
  C.getSuggestions port { line, moduleInfo: { modules, getQualifiedModule, mainModule: state.main }}
  where
  raiseError' :: (Array C.AtomSuggestion) -> Error -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Array C.AtomSuggestion)
  raiseError' x e = do
    liftEff $ raiseError e
    pure x
  liftEff'' :: forall a. Eff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) a -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) a
  liftEff'' = liftEff

useEditor :: forall eff. Int -> (Ref State) -> TextEditor
  -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE, config :: CONFIG | eff) Unit
useEditor port modulesStateRef editor = do
  path <- getPath editor
  text <- getText editor
  let mainModule = getMainModule text
  case path, mainModule of
    Just path', Just m -> void $ runAff logError ignoreError $ do
      -- We load all deps initially, but only post 0.8.4, and maybe something resets psc-ide state
      loadDeps port m
      state <- getModulesForFile port path' text
      liftEff $ writeRef modulesStateRef state
      pure unit
    _, _ -> pure unit

useEditor' :: forall eff. (Ref State) -> Maybe Int -> Maybe TextEditor
  -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE, config :: CONFIG | eff) Unit
useEditor' modulesStateRef port editor = do
  case port, editor of
    Just port', Just editor' -> useEditor port' modulesStateRef editor'
    _, _ -> pure unit

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
  , err :: EXCEPTION
  , dom :: DOM
  , grammar :: GRAMMAR
  , random :: RANDOM
  )

main :: Eff MainEff Foreign
main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  messagesRef <- newRef ([] :: Array AtomLintMessage)
  linterInternalRef <- newRef (Nothing :: Maybe LinterInternal)
  buildStatusRef <- newRef (Nothing :: Maybe Element)

  startedRef <- newRef (false :: Boolean)
  serversRef <- newRef (empty :: StrMap.StrMap { port :: Int, quit :: Eff MainEff Unit })

  let
    getPort :: String -> Eff MainEff (Maybe { root :: String, port :: Int })
    getPort file = runMaybeT $ do
      servers <- liftEff $ readRef serversRef
      root <- MaybeT $ getRoot file
      { port } <- MaybeT $ pure $ lookup root servers
      pure { root, port }

    doLint :: (Maybe String) -> Eff MainEff Unit
    doLint file = do
      linterIndie <- readRef linterIndieRef
      statusElt <- readRef buildStatusRef
      portRes <- maybe (pure Nothing) getPort file
      case portRes, linterIndie, statusElt of
        -- TODO if no psc-ide port, should still be able to do FULL build
        Just { port, root }, Just linterIndie', Just statusElt' -> void $ runAff raiseError ignoreError $ do
          messages <- lint (Just port) file atom.config root linterIndie' statusElt'
          liftEff $ maybe (pure unit) (writeRef messagesRef) messages
          P.load port [] []
          editor <- liftEff $ getActiveTextEditor atom.workspace
          liftEff $ useEditor' modulesState (Just port) editor
        _, _, _ -> pure unit

    quickFix :: Eff MainEff Unit
    quickFix = do
      messages <- readRef messagesRef
      editor <- getActiveTextEditor atom.workspace
      linter <- readRef linterInternalRef
      case { editor, linter, n: length messages } of
        { editor: Just e, linter: Just l, n } | n > 0 -> withPort \port -> showQuickFixes port modulesState e l messages
        _ -> pure unit

    restartPscIdeServer :: Eff MainEff Unit
    restartPscIdeServer = do
      deactivate
      startPscIdeServer

    startPscIdeServer :: Eff MainEff Unit
    startPscIdeServer = void $ runMaybeT do
      ed <- MaybeT $ getActiveTextEditor atom.workspace
      path <- MaybeT $ getPath ed
      liftEff $ log $ "Starting psc-ide-server for path: " <> path
      liftEff $ void $ runAff
                (\_ -> log "Error starting server")
                ignoreError
                (startPscIdeServer' path)

    startPscIdeServer' :: String -> Aff MainEff Unit
    startPscIdeServer' path = do
      root' <- liftEff $ getRoot path
      case root' of
        Nothing -> pure unit
        Just root -> do
          { port, quit } <- startServer root
          maybe (pure unit) (\p -> void $ P.load p [] []) port
          liftEff $ do
            editor <- getActiveTextEditor atom.workspace
            useEditor' modulesState port editor
            registerTooltips getPortActiveEditor modulesState
            case port of
              Nothing -> pure unit
              Just port' -> modifyRef serversRef $ StrMap.insert root { port: port', quit}

    getPortActiveEditor :: Eff MainEff (Maybe Int)
    getPortActiveEditor = runMaybeT do
      editor <- MaybeT $ getActiveTextEditor atom.workspace
      file <- MaybeT $ getPath editor
      _.port <$> (MaybeT $ getPort file)

    withPortDef :: forall a. Eff MainEff a -> (Int -> Eff MainEff a) -> Eff MainEff a
    withPortDef def e = getPortActiveEditor >>= maybe def e

    withPort :: (Int -> Eff MainEff Unit) -> Eff MainEff Unit
    withPort = withPortDef (pure unit)

    activate :: Eff MainEff Unit
    activate = do
      let cmd name action = addCommand atom.commands "atom-workspace" ("ide-purescript:"<>name) (const action)
      cmd "build" $ doLint Nothing
      cmd "show-quick-fixes" quickFix
      cmd "pursuit-search" $ withPort pursuitSearch
      cmd "pursuit-search-modules" $ withPort $ \port -> pursuitSearchModule port modulesState
      cmd "add-module-import" $ withPort $ \port -> addModuleImportCmd port modulesState
      cmd "add-explicit-import" $ withPort $ \port -> addExplicitImportCmd port modulesState
      cmd "search" $  withPort $ \port -> localSearch port modulesState
      cmd "case-split" $ withPort caseSplit
      cmd "add-clause" $ withPort addClause
      cmd "fix-typo" $ withPort $ fixTypo modulesState
      cmd "goto-definition" $ withPort $ gotoDef modulesState
      cmd "restart-psc-ide" $ restartPscIdeServer
      registerCommands

      installDependencies

      observeTextEditors atom.workspace (\editor -> do
        path <- getPath editor
        case path of
          Just path' | contains ".purs" path' -> do
            onDidSave editor (\_ -> do
              buildOnSave <- getConfig atom.config "ide-purescript.buildOnSave"
              let buildOnSaveEnabled = either (const false) id $ readBoolean buildOnSave
              when buildOnSaveEnabled (doLint path)
            )
          _ -> pure unit
      )

      onDidChangeActivePaneItem atom.workspace (\item -> void $ runMaybeT do
        editor <- MaybeT $ pure $ toEditor item
        path <- MaybeT $ getPath editor
        port <- liftEff $ getPort path
        liftEff $ case port of
          Just { port: port' } -> do
            log $ "Switching to editor for file: " <> path
            useEditor' modulesState (Just port') (toEditor item)
          Nothing -> startPscIdeServer
      )

      startPscIdeServer
      Psci.activate
      writeRef startedRef true

    deactivate :: Eff MainEff Unit
    deactivate = do
      servers <- readRef serversRef
      sequence $ _.quit <$> StrMap.values servers
      writeRef serversRef empty

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
            withPortDef (Promise.fromAff $ pure []) (\p -> getSuggestions p state x)
        , onDidInsertSuggestion: mkEffFn1 \x -> do
            shouldAddImport <- either (const false) id <$> readBoolean <$> getConfig atom.config "ide-purescript.autocomplete.addImport"
            withPort \port ->
              when shouldAddImport (void $ runAff raiseError ignoreError $ addSuggestionImport port modulesState x)
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
