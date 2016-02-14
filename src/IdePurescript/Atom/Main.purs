module IdePurescript.Atom.Main where

import Prelude
import Atom.Atom
import Atom.NotificationManager
import Atom.CommandRegistry
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.LinterBuild
import Data.Maybe(maybe,Maybe(..))
import IdePurescript.Atom.Hooks.Linter

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Function.Eff
import IdePurescript.Modules
import Control.Monad.Aff
import Atom.Workspace
import Control.Monad.Eff.Console
import IdePurescript.PscIde
import Atom.Editor
import Atom.Range
import Atom.Point
import PscIde (NET)
import Control.Monad.Eff.Class (liftEff)
import IdePurescript.Atom.Completion as C
import Control.Promise

-- atom.commands.add("atom-workspace", "purescript:pursuit-search", @pursuit.search)
-- atom.commands.add("atom-workspace", "purescript:pursuit-search-modules", @pursuit.searchModule)
-- atom.commands.add("atom-workspace", "purescript:build", @lint)
-- atom.commands.add("atom-workspace", "purescript:show-quick-fixes", @quickfix)
--
-- atom.commands.add("atom-workspace", "purescript:add-module-import", =>


getSuggestions :: forall eff a. State -> { editor :: TextEditor, bufferPosition :: Point }
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
    Just main -> runAff ignoreError ignoreError $ do
      loadDepsA main
      state <- getModulesForFile path text
      liftEff $ writeRef modulesStateRef state
      pure unit
    Nothing -> pure unit

main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  modulesState <- newRef (initialModulesState)
  addCommand atom.commands "atom-workspace" "purescript:build" $ (\_ -> do
    addInfo atom.notifications "Building PureScript"
    root <- getProjectRoot
    linterIndie <- readRef linterIndieRef
    case { root, linterIndie } of
      { root: Just root', linterIndie: Just linterIndie' } -> runAff ignoreError ignoreError $ do
        messages <- lint atom.config root' linterIndie'
        -- TODO: update modules state, quick fixes
        pure unit
      _ -> pure unit)

  observeTextEditors atom.workspace (\editor ->
    useEditor modulesState editor
  )
  onDidChangeActivePaneItem atom.workspace (\item ->
    maybe (pure unit) (useEditor modulesState) (toEditor item)
  )

  pure
    { config
    , consumeLinterIndie: mkEffFn1 \registry -> do
        linterIndie <- register registry {name: "PureScript"}
        writeRef linterIndieRef (Just linterIndie)
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
