module IdePurescript.Atom.Main where

import Prelude
import Control.Promise as Promise
import IdePurescript.Atom.Completion as C
import IdePurescript.Atom.Psci as Psci
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (EDITOR, TextEditor, toEditor, onDidSave, getText, getPath, getTextInRange, setTextInBufferRange, setText, getBuffer, getCursorBufferPosition)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Point (Point, getRow, getColumn, mkPoint)
import Atom.Project (PROJECT)
import Atom.Range (mkRange, Range, getStart, getEnd)
import Atom.TextBuffer (setTextViaDiff)
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
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT, lift)
import Control.Promise (Promise)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (length)
import Data.Either (either, Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (readBoolean)
import Data.Function.Eff (mkEffFn1)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.BuildStatus (getBuildStatus)
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.Hooks.Dependencies (installDependencies)
import IdePurescript.Atom.Hooks.Linter (LinterInternal, LinterIndie, LINTER, register)
import IdePurescript.Atom.Hooks.StatusBar (addLeftTile)
import IdePurescript.Atom.LinterBuild (lint, getProjectRoot)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.PscIdeServer (startServer)
import IdePurescript.Atom.QuickFixes (showQuickFixes)
import IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic)
import IdePurescript.Atom.Tooltips (registerTooltips, getToken)
import IdePurescript.Modules (State, ImportResult(AmbiguousImport, UpdatedImports), getQualModule, addModuleImport, addExplicitImport, initialModulesState, getModulesForFile, getMainModule)
import IdePurescript.PscIde (getPursuitModuleCompletion, getPursuitCompletion, loadDeps, getAvailableModules, getCompletion, eitherToErr, getLoadedModules)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import PscIde (NET)
import PscIde.Command (Completion(..))

getSuggestions :: forall eff. State -> { editor :: TextEditor, bufferPosition :: Point }
  -> Eff (editor :: EDITOR, net :: NET, note :: NOTIFY | eff) (Promise (Array C.AtomSuggestion))
getSuggestions state ({editor, bufferPosition}) = Promise.fromAff $ flip catchError (raiseError' []) $ do
  let range = mkRange (mkPoint (getRow bufferPosition) 0) bufferPosition
  line <- liftEff $ getTextInRange editor range
  modules <- getLoadedModules -- getUnqualActiveModules state
  let getQualifiedModule = (flip getQualModule) state
  C.getSuggestions { line, moduleInfo: { modules, getQualifiedModule }}
  where
  raiseError' :: (Array C.AtomSuggestion) -> Error -> Aff (editor :: EDITOR, net :: NET, note :: NOTIFY | eff) (Array C.AtomSuggestion)
  raiseError' x e = do
    liftEff $ raiseError e
    pure x


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

main :: _
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
    pursuitSearch = selectListViewDynamic view (\x -> log x.identifier) Nothing (const "") getPursuitCompletion 1000
      where
      view {identifier, "type": ty, "module": mod, package} =
         "<li class='two-lines'>"
         ++ "<div class='primary-line'>" ++ identifier ++ ": <span class='text-info'>" ++ ty ++ "</span></div>"
         ++ "<div class='secondary-line'>" ++ mod ++ " (" ++ package ++ ")</div>"
         ++ "</li>"

    pursuitSearchModule :: Eff MainEff Unit
    pursuitSearchModule = selectListViewDynamic view importDialog (Just "module") id getPursuitModuleCompletion 1000
      where
      view {"module": mod, package} =
         "<li class='two-lines'>"
         ++ "<div class='primary-line'>" ++ mod ++ "</span></div>"
         ++ "<div class='secondary-line'>" ++ package ++ "</div>"
         ++ "</li>"
      importDialog :: forall a. {"module" :: String | a} -> Eff MainEff Unit
      importDialog {"module": mod} = selectListViewStatic textView (doImport mod) Nothing ["Import module", "Cancel"]
        where
        textView x = "<li>" ++ x ++ "</li>"
        doImport mod x = when (x == "Import module") $ addImport mod

    addModuleImportCmd :: Eff MainEff Unit
    addModuleImportCmd = runAff raiseError ignoreError do
      modules <- getAvailableModules
      liftEff $ selectListViewStatic view addImport Nothing modules
      where
        view x = "<li>" ++ x ++ "</li>"

    addExplicitImportCmd :: Eff MainEff Unit
    addExplicitImportCmd = runAff raiseError ignoreError do
      editor <- liftEff $ getActiveTextEditor atom.workspace
      case editor of
        Just ed -> do
          { line, col, pos, range } <- liftEff $ getLinePosition ed
          promptText <- liftEff $ maybe "" _.word <$> getToken ed pos
          res <- addPromptPanel "Identifier" promptText
          maybe (pure unit) (addIdentImport Nothing) res
        Nothing -> pure unit

    addIdentImport :: Maybe String -> String -> Aff MainEff Unit
    addIdentImport moduleName ident = do
      editor <- liftEff $ getActiveTextEditor atom.workspace
      maybe (pure unit) (addIdentImport' moduleName ident) editor

    addIdentImport' :: Maybe String -> String -> TextEditor -> Aff MainEff Unit
    addIdentImport' moduleName ident editor = do
      text <- liftEff $ getText editor
      path <- liftEff $ getPath editor
      state <- liftEff $ readRef modulesState
      { state: newState, result: output} <- addExplicitImport state path text moduleName ident
      liftEff $ writeRef modulesState newState
      liftEff $ case output of
        UpdatedImports out -> do
          buf <- getBuffer editor
          void $ setTextViaDiff buf out
        AmbiguousImport opts -> do
          selectListViewStatic view addImp Nothing (runCompletion <$> opts)
        _ -> pure unit
      where
      runCompletion (Completion obj) = obj
      -- TODO nicer if we can make select view aff-ish
      addImp { identifier, "module'": m } = runAff raiseError ignoreError $ addIdentImport (Just m) identifier
      view {identifier, "module'": m} = "<li>" ++ m ++ "." ++ identifier ++ "</li>"

    addSuggestionImport :: { editor :: TextEditor, suggestion :: C.AtomSuggestion } -> Aff MainEff Unit
    addSuggestionImport { editor, suggestion: { addImport: Just { mod, identifier, qualifier: Nothing } } } =
      addIdentImport' (Just mod) identifier editor
    addSuggestionImport _ = pure unit

    addImport :: String -> Eff MainEff Unit
    addImport moduleName = do
      maybeEditor <- getActiveTextEditor atom.workspace
      state <- liftEff $ readRef modulesState
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          text <- getText editor
          path <- getPath editor
          runAff raiseError ignoreError $ do
            output <- addModuleImport state path text moduleName
            liftEff $ maybe (pure unit) (void <<< setText editor <<< _.result) output

    localSearch :: Eff MainEff Unit
    localSearch = selectListViewDynamic view (\x -> log x.identifier) Nothing (const "") search 50
      where
      search text = do
        state <- liftEff $ readRef modulesState
        modules <- getLoadedModules
        let getQualifiedModule = (flip getQualModule) state
        getCompletion text "" false modules getQualifiedModule

      view {identifier, "type": ty, "module": mod} =
         "<li class='two-lines'>"
         ++ "<div class='primary-line'>" ++ identifier ++ ": <span class='text-info'>" ++ ty ++ "</span></div>"
         ++ "<div class='secondary-line'>" ++ mod ++ "</div>"
         ++ "</li>"

    getLinePosition :: TextEditor -> Eff MainEff { line :: String, col :: Int, pos :: Point, range :: Range }
    getLinePosition ed = do
      pos <- getCursorBufferPosition ed
      let range = mkRange (mkPoint (getRow pos) 0) (mkPoint (getRow pos) 1000)
          col = getColumn pos
      line <- getTextInRange ed range
      pure { line, pos, col, range }

    caseSplit :: Eff MainEff Unit
    caseSplit = do
      runAff raiseError ignoreError $ runMaybeT body
      where
      body :: MaybeT (Aff MainEff) Unit
      body = do
        ed :: TextEditor <- MaybeT $ liftEff $ getActiveTextEditor atom.workspace
        { line, col, pos, range } <- lift $ liftEff $ getLinePosition ed
        { range: wordRange } <- MaybeT $ liftEff $ getToken ed pos
        ty <- MaybeT $ addPromptPanel "Parameter type" ""
        lines <- lift $ eitherToErr $ P.caseSplit line (getColumn $ getStart wordRange) (getColumn $ getEnd wordRange) true ty
        lift $ void $ liftEff $ setTextInBufferRange ed range $ intercalate "\n" lines

    addClause :: Eff MainEff Unit
    addClause = do
      editor <- getActiveTextEditor atom.workspace
      case editor of
        Just ed ->
          runAff raiseError ignoreError $ do
            { line, col, range } <- liftEff $ getLinePosition ed
            lines <- eitherToErr $ P.addClause line true
            liftEff $ setTextInBufferRange ed range $ intercalate "\n" lines
        Nothing -> pure unit

    fixTypo :: Eff MainEff Unit
    fixTypo = do
      runAff raiseError ignoreError $ runMaybeT body
      where
      body :: MaybeT (Aff MainEff) Unit
      body = do
        ed :: TextEditor <- MaybeT $ liftEff $ getActiveTextEditor atom.workspace
        { line, col, pos, range } <- lift $ liftEff $ getLinePosition ed
        { word, range: wordRange } <- MaybeT $ liftEff $ getToken ed pos
        corrections <- lift $ eitherToErr (P.suggestTypos word 2)
        liftEff $ selectListViewStatic view (replaceTypo ed wordRange) Nothing (runCompletion <$> corrections)
        where
          runCompletion (Completion obj) = obj
          replaceTypo ed wordRange { identifier } = void $ unsafeInterleaveEff (setTextInBufferRange ed wordRange identifier)
          view {identifier, "module'": m} = "<li>" ++ m ++ "." ++ identifier ++ "</li>"
          getIdentFromCompletion (Completion c) = c.identifier

    activate :: Eff MainEff Unit
    activate = do
      let cmd name action = addCommand atom.commands "atom-workspace" ("purescript:"++name) (const action)
      cmd "build" doLint
      cmd "show-quick-fixes" quickFix
      cmd "pursuit-search" pursuitSearch
      cmd "pursuit-search-modules" pursuitSearchModule
      cmd "add-module-import" addModuleImportCmd
      cmd "add-explicit-import" addExplicitImportCmd
      cmd "search" localSearch
      cmd "case-split" caseSplit
      cmd "add-clause" addClause
      cmd "fix-typo" fixTypo

      installDependencies

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
        , onDidInsertSuggestion: mkEffFn1 \x -> do
            shouldAddImport <- getConfig atom.config "ide-purescript.importOnAutocomplete"
            when (readBoolean shouldAddImport == Right true)
              (runAff raiseError ignoreError $ addSuggestionImport x)
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
