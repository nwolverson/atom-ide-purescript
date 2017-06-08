module IdePurescript.Atom.Main where

import Prelude
import Control.Promise as Promise
import Data.StrMap as StrMap
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Psci as Psci
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand, dispatchRoot)
import Atom.Config (CONFIG, getConfig)
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
import Control.Monad.Eff.Uncurried (mkEffFn1, mkEffFn2, runEffFn1)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Promise (Promise)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (sequence_, traverse_)
import Data.Foreign (Foreign, readBoolean, toForeign)
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Nullable (toNullable)
import Data.StrMap (lookup, empty)
import Data.String (Pattern(Pattern), contains)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import IdePurescript.Atom.Assist (gotoDefHyper, fixTypo, addClause, caseSplit, gotoDef)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LINTER, LinterIndie, RegisterIndie)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.Imports (addSuggestionImport, addExplicitImportCmd, addModuleImportCmd)
import IdePurescript.Atom.LinterBuild (lint)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.Psci (registerCommands)
import IdePurescript.Atom.Search (localSearch, pursuitSearchModule, pursuitSearch)
import IdePurescript.Atom.Tooltips (getToken, registerTooltips, showTooltipAtCursor)
import IdePurescript.Modules (State, getModulesForFile, getQualModule, getUnqualActiveModules, initialModulesState)
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
  line <- liftEff $ getTextInRange editor range
  atom <- liftEff getAtom
  configRaw <- liftEff $ getConfig atom.config "ide-purescript.autocomplete.allModules"
  let autoCompleteAllModules = either (const false) id $ runExcept $ readBoolean configRaw
  modules <- if activatedManually || autoCompleteAllModules then getLoadedModules port else pure $ getUnqualActiveModules state Nothing
  let getQualifiedModule = (flip getQualModule) state
  C.getSuggestions port { line, moduleInfo: { modules, getQualifiedModule, mainModule: state.main }}
  where
  raiseError' :: (Array C.AtomSuggestion) -> Error -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY, config :: CONFIG | eff) (Array C.AtomSuggestion)
  raiseError' x e = do
    liftEff $ raiseError e
    pure x

useEditor :: forall eff. Int -> (Ref State) -> TextEditor
  -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE, config :: CONFIG | eff) Unit
useEditor port modulesStateRef editor = do
  path <- getPath editor
  when (maybe false isPursFile path) do
    text <- getText editor
    maybe (pure unit) (\path' -> void $ runAff logError ignoreError $ do
      state <- getModulesForFile port path' text
      liftEff $ writeRef modulesStateRef state) path

useEditor' :: forall eff. (Ref State) -> Maybe Int -> Maybe TextEditor
  -> Eff (editor ::EDITOR, net :: NET, ref :: REF, console :: CONSOLE, config :: CONFIG | eff) Unit
useEditor' modulesStateRef port editor = do
  case port, editor of
    Just port', Just editor' -> useEditor port' modulesStateRef editor'
    _, _ -> pure unit

isPursFile :: String -> Boolean
isPursFile path = contains (Pattern ".purs") path

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
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  buildStatusRef <- newRef (Nothing :: Maybe Element)

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
          initialEditor <- liftEff $ getActiveTextEditor atom.workspace
          lint (_.port <$> portRes) modulesState file initialEditor atom.config root linterIndie' statusElt'
          editor <- liftEff $ getActiveTextEditor atom.workspace
          liftEff $ useEditor' modulesState (_.port <$> portRes) editor
        _, _, _ -> pure unit

    quickFix :: Eff MainEff Unit
    quickFix = do
      dispatchRoot atom.commands "intentions:show"

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
    startPscIdeServer' path = when (isPursFile path) do
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
              traverse_ (\p -> do
                r <- P.load p [] []
                case r of
                  Left err -> liftEff $ raiseError $ Exception.error err
                  Right _  -> pure unit
              ) port
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
              delay (Milliseconds 500.0)
              a

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

    withPortState f = withPort (\port -> f port modulesState)

    activate :: Eff MainEff Unit
    activate = do
      log "PureScript: Activating!"
      let cmd name action = addCommand atom.commands "atom-workspace" ("ide-purescript:"<>name) (const action)
      cmd "build" $ doLint Nothing
      cmd "show-quick-fixes" quickFix
      cmd "pursuit-search" $ withPort pursuitSearch
      cmd "pursuit-search-modules" $ withPortState pursuitSearchModule
      cmd "add-module-import" $ withPortState addModuleImportCmd
      cmd "add-explicit-import" $ withPortState addExplicitImportCmd
      cmd "search" $  withPort $ \port -> localSearch port modulesState
      cmd "case-split" $ withPort caseSplit
      cmd "add-clause" $ withPort addClause
      cmd "fix-typo" $ withPort $ fixTypo modulesState
      cmd "goto-definition" $ withPort $ gotoDef modulesState
      cmd "restart-psc-ide" $ restartPscIdeServer
      cmd "show-tooltip" $ withPortState showTooltipAtCursor
      registerCommands

      installDependencies

      observeTextEditors atom.workspace \editor -> do
        onDidSave editor $ const do
          getPath editor >>= case _ of
            Just path | isPursFile path -> do
              buildOnSave <- getConfig atom.config "ide-purescript.buildOnSave"
              let buildOnSaveEnabled = either (const false) id $ runExcept $ readBoolean buildOnSave
              when buildOnSaveEnabled (doLint $ Just path)
            _ -> pure unit

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

    deactivate :: Eff MainEff Unit
    deactivate = do
      servers <- readRef serversRef
      sequence_ $ _.quit <$> StrMap.values servers
      writeRef serversRef empty

  pure $ toForeign
    { config
    , activate: mkEffFn1 \_ -> activate
    , deactivate: mkEffFn1 \_ -> deactivate
    , consumeLinterIndie: mkEffFn1 \(register:: RegisterIndie) -> do
        linterIndie <- runEffFn1 register { name: "PureScript" }
        writeRef linterIndieRef $ Just linterIndie
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
