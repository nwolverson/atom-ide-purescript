module IdePurescript.Atom.Search where

import Prelude hiding (div)

import Atom.Atom (getAtom)
import Atom.NotificationManager (NOTIFY)
import Atom.Types (EDITOR)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (either)
import Data.Foreign (readArray, toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.Imports (addModuleImport')
import IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic)
import LanguageServer.IdePurescript.Search (SearchResult(..), decodeSearchResult)
import Network.HTTP.Affjax (AJAX, Affjax, affjax, defaultRequest)
import Text.Smolder.HTML (div, li, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (MarkupM, text, (!))
import Text.Smolder.Renderer.String (render)
import IdePurescript.Pursuit (PursuitSearchInfo(..), PursuitSearchResult(..), pursuitModuleSearchRequest, pursuitSearchRequest)

twoLines :: forall a. MarkupM a Unit -> MarkupM a Unit -> MarkupM a Unit
twoLines line1 line2 = li ! className "two-lines" $ do
  div ! className "first-line" $ line1
  div ! className "second-line" $ line2

pursuitSearch :: forall eff. Eff (LocalEff eff) Unit
pursuitSearch =
  selectListViewDynamic view (\(PursuitSearchResult { text }) -> log text) Nothing (const "") pursuitSearchRequest 1000
  where
  view (PursuitSearchResult { package, text: txt, info : PursuitSearchInfo { title: Just title, typ, mod: Just mod, typeText } }) = render $
    twoLines
      do text title
         case typeText of
            Just tt -> do
              text " :: "
              span ! className "text-info" $ text $ tt
            _ -> text ""
      do text $ mod <> " (" <> package <> ")"
  view _ = ""

pursuitSearchModule :: forall eff. LanguageClientConnection -> Eff (LocalEff eff) Unit
pursuitSearchModule conn =
  selectListViewDynamic view importDialog Nothing (const "") pursuitModuleSearchRequest 1000
  where

  view (PursuitSearchResult { package, info: PursuitSearchInfo { mod: Just mod } }) = render $
    twoLines (text mod) (text package)
  view _ = ""

  importDialog :: PursuitSearchResult -> Eff (LocalEff eff) Unit
  importDialog (PursuitSearchResult { info: PursuitSearchInfo { mod: Just mod } }) =
    selectListViewStatic textView (doImport mod) Nothing ["Import module", "Cancel"]
    where
    textView x = render $ li $ text x
    doImport mod x = when (x == "Import module") $ do
      atom <- getAtom
      editor <- getActiveTextEditor atom.workspace
      maybe (pure unit) (\e -> addModuleImport' conn e mod) editor
  importDialog _ = pure unit

type LocalEff eff = (exception :: EXCEPTION, note :: NOTIFY, console :: CONSOLE, dom :: DOM, ajax :: AJAX, editor :: EDITOR, workspace :: WORKSPACE | eff)

localSearch :: forall eff. LanguageClientConnection -> Eff (LocalEff eff) Unit
localSearch conn =
  selectListViewDynamic view (\(SearchResult { identifier }) -> log identifier) Nothing (const "") search 50

  where
    search text = do
      results <- executeCommand conn { command: "purescript.search", arguments: [ toForeign text ] }
      let results' = readArray results >>= traverse decodeSearchResult
      pure $ either (const []) id $ runExcept $ results'

    view (SearchResult { identifier, typ, mod }) = render do
      twoLines do text identifier
                  text ": "
                  span ! className "text-info" $ text typ
               do text mod
