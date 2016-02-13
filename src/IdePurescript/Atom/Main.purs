module IdePurescript.Atom.Main where

import Prelude
import Atom.Atom
import Atom.NotificationManager
import Atom.CommandRegistry
import IdePurescript.Atom.Config (config)
import IdePurescript.Atom.LinterBuild
import Data.Maybe(maybe,Maybe(..))
import IdePurescript.Atom.Hooks.Linter

import Control.Monad.Eff.Ref
import Data.Function.Eff
-- atom.commands.add("atom-workspace", "purescript:pursuit-search", @pursuit.search)
-- atom.commands.add("atom-workspace", "purescript:pursuit-search-modules", @pursuit.searchModule)
-- atom.commands.add("atom-workspace", "purescript:build", @lint)
-- atom.commands.add("atom-workspace", "purescript:show-quick-fixes", @quickfix)
--
-- atom.commands.add("atom-workspace", "purescript:add-module-import", =>


main = do
  atom <- getAtom
  linterIndieRef <- newRef (Nothing :: Maybe LinterIndie)
  addCommand atom.commands "atom-workspace" "purescript:build" $ (\_ -> do
    addInfo atom.notifications "Building PureScript"
    root <- getProjectRoot
    linterIndie <- readRef linterIndieRef
    case { root, linterIndie } of
      { root: Just root', linterIndie: Just linterIndie' } -> lint atom.config root' linterIndie'
      _ -> pure unit)
  pure {
    config,
    consumeLinterIndie: mkEffFn1 \registry -> do
      linterIndie <- register registry {name: "PureScript"}
      writeRef linterIndieRef (Just linterIndie)
  }
