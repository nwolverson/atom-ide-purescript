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
import Control.Monad.Aff (attempt, Aff, runAff, later')
import Control.Monad.Aff.AVar (putVar, takeVar, makeVar', AVAR)
import Control.Monad.Aff.Internal (AVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, modifyRef, newRef)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Promise (Promise)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Either (Either(Left, Right), either)
import Data.Foreign (Foreign, readBoolean, toForeign)
import Data.Function.Eff (mkEffFn2, mkEffFn1)
import Data.Maybe (isJust, Maybe(Just, Nothing), maybe)
import Data.Nullable (toNullable)
import Data.StrMap (lookup, empty)
import Data.String (Pattern(Pattern), contains)
import Data.Traversable (sequence)
import IdePurescript.Atom.Assist (gotoDefHyper, fixTypo, addClause, caseSplit, gotoDef)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LinterInternal, LinterIndie, LINTER, register)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Imports (addSuggestionImport, addExplicitImportCmd, addModuleImportCmd)
import IdePurescript.Atom.LinterBuild (lint)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.Psci (registerCommands)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Atom.Search (localSearch, pursuitSearchModule, pursuitSearch)
import IdePurescript.Atom.Tooltips (getToken, registerTooltips)
import IdePurescript.Modules (State, getQualModule, initialModulesState, getModulesForFile, getMainModule, getUnqualActiveModules)
import IdePurescript.PscIde (getLoadedModules)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)
import PscIde.Project (getRoot)

getSuggestions :: forall eff. Int -> State -> { editor :: TextEditor, bufferPosition :: Point, activatedManually :: Boolean }
  -> Eff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Promise (Array C.AtomSuggestion))
getSuggestions port state ({editor, bufferPosition, activatedManually}) = Promise.fromAff $ flip catchError (raiseError' []) $ do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- liftEff'' $ getTextInRange editor range
  atom <- liftEff'' getAtom
  configRaw <- liftEff'' $ getConfig atom.config "ide-purescript.autocomplete.allModules"
  let autoCompleteAllModules = either (const false) id $ runExcept $ readBoolean configRaw
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
  log "PureScript: Starting!"
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  messagesRef <- newRef ([] :: Array AtomLintMessage)
  linterInternalRef <- newRef (Nothing :: Maybe LinterInternal)
  buildStatusRef <- newRef (Nothing :: Maybe Element)

  startedRef <- newRef (false :: Boolean)
  serversRef <- newRef (empty :: StrMap.StrMap { port :: Int, quit :: Eff MainEff Unit })
  startingV <- newRef (Nothing :: Maybe (AVar Unit))

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
      root' <- case portRes of
        Just { root } -> pure $ Just root
        Nothing -> runMaybeT $ getPathActiveEditor >>= getRoot >>> MaybeT
      case root', linterIndie, statusElt of
        Just root, Just linterIndie', Just statusElt' -> void $ runAff raiseError ignoreError $ do
          messages <- lint (_.port <$> portRes) file atom.config root linterIndie' statusElt'
          liftEff $ maybe (pure unit) (writeRef messagesRef) messages
          editor <- liftEff $ getActiveTextEditor atom.workspace
          liftEff $ useEditor' modulesState (_.port <$> portRes) editor
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
      path <- getPathActiveEditor
      liftEff $ log $ "Starting psc-ide-server for path: " <> path
      liftEff $ void $ runAff
                (\e -> log $ "Error starting server: " <> show e)
                ignoreError
                (startPscIdeServer' path)

    getVar :: Aff MainEff (AVar Unit)
    getVar = liftEff (readRef startingV) >>=
      maybe
        do
          v <- makeVar' unit
          liftEff $ writeRef startingV $ Just v
          pure v
        pure

    startPscIdeServer' :: String -> Aff MainEff Unit
    startPscIdeServer' path = do
      var <- getVar
      takeVar var
      root' <- liftEff $ getRoot path
      case root' of
        Nothing -> pure unit
        Just root -> do
          servers <- liftEff $ readRef serversRef
          when (isJust $ lookup root servers) $ liftEff $ log $ "Not starting server - already started: " <> path
          unless (isJust $ lookup root servers) do
            { port, quit } <- startServer root
            liftEff $ log $ "Loading modules: " <> path
            retry do
              maybe (pure unit) (\p -> void $ P.load p [] []) port
              liftEff $ do
                editor <- getActiveTextEditor atom.workspace
                useEditor' modulesState port editor
                registerTooltips getPortActiveEditor modulesState
                case port of
                  Nothing -> pure unit
                  Just port' -> modifyRef serversRef $ StrMap.insert root { port: port', quit}
      liftEff $ log $ "Finished initialising server: " <> path
      putVar var unit
      where
        retry :: Aff MainEff Unit -> Aff MainEff Unit
        retry a = do
          res <- attempt a
          case res of
            Right r -> pure r
            Left err -> do
              liftEff $ log $ "Retrying starting server after 500ms: " <> show err
              later' 500 a

    getPathActiveEditor :: MaybeT (Eff MainEff) String
    getPathActiveEditor = do
      editor <- MaybeT $ getActiveTextEditor atom.workspace
      MaybeT $ getPath editor

    getPortActiveEditor :: Eff MainEff (Maybe Int)
    getPortActiveEditor = runMaybeT do
      file <- getPathActiveEditor
      _.port <$> (MaybeT $ getPort file)

    withPortDef :: forall a. Eff MainEff a -> (Int -> Eff MainEff a) -> Eff MainEff a
    withPortDef def e = getPortActiveEditor >>= maybe def e

    withPort :: (Int -> Eff MainEff Unit) -> Eff MainEff Unit
    withPort = withPortDef (pure unit)

    activate :: Eff MainEff Unit
    activate = do
      log "PureScript: Activating!"
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
          Just path' | contains (Pattern ".purs") path' -> do
            onDidSave editor (\_ -> do
              buildOnSave <- getConfig atom.config "ide-purescript.buildOnSave"
              let buildOnSaveEnabled = either (const false) id $ runExcept $ readBoolean buildOnSave
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
    , provideAutocomplete: mkEffFn1 \_ -> do
        excludeLowerPriority <- either (const true) id <$> runExcept <$> readBoolean <$> getConfig atom.config "ide-purescript.autocomplete.excludeLowerPriority"
        pure {
          selector: ".source.purescript"
        , disableForSelector: ".source.purescript .comment, .source.purescript .string"
        , inclusionPriority: 1
        , excludeLowerPriority
        , getSuggestions: mkEffFn1 $ \x -> do
            state <- readRef modulesState
            withPortDef (Promise.fromAff $ pure []) (\p -> getSuggestions p state x)
        , onDidInsertSuggestion: mkEffFn1 \x -> do
            shouldAddImport <- either (const false) id <$> runExcept <$> readBoolean <$> getConfig atom.config "ide-purescript.autocomplete.addImport"
            withPort \port ->
              when shouldAddImport (void $ runAff raiseError ignoreError $ addSuggestionImport port modulesState x)
        }
    , provideHyperclick: \_ ->
      {
        getSuggestion: mkEffFn2 \editor pos -> do
          rangeM <- getToken editor pos
          pure $ toNullable $ (_ <$> rangeM) \{range} ->
            {
              range,
              callback: withPort \port -> gotoDefHyper modulesState port editor pos
            }
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
