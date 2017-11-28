module IdePurescript.Atom.Search where

import Prelude hiding (div)

import Atom.Atom (getAtom)
import Atom.NotificationManager (NOTIFY)
import Atom.Types (EDITOR)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Data.Argonaut.Decode ((.??))
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Foreign (readArray, toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Traversable (traverse)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.Imports (addModuleImport')
import IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic)
import LanguageServer.IdePurescript.Search (SearchResult(..), decodeSearchResult)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, affjax, defaultRequest, get)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Text.Smolder.HTML (br, div, li, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.String (render)

newtype PursuitSearchInfo = PursuitSearchInfo
  { typeOrValue :: Maybe String
  , mod :: Maybe String
  , typeText :: Maybe String
  , title :: Maybe String
  , typ :: String
  }

instance decodeJsonPursuitSearchInfo :: DecodeJson PursuitSearchInfo where
  decodeJson json =
    do
      obj <- decodeJson json
      typeOrValue <- obj .?? "typeOrValue"
      mod <- obj .?? "module"
      typeText <- pure $ hush $ obj .? "typeText"
      title <- obj .?? "title"
      typ <- obj .? "type"
      pure $ PursuitSearchInfo { typeOrValue, mod, typeText, title, typ }

newtype PursuitSearchResult = PursuitSearchResult
  { text :: String
  , markup :: String
  , url :: String
  , version :: String
  , package :: String
  , info :: PursuitSearchInfo
  }

instance decodeJsonPursuitSearchResult :: DecodeJson PursuitSearchResult where
  decodeJson json =
    do
      obj <- decodeJson json
      text <- obj .? "text"
      markup <- obj .? "markup"
      url <- obj .? "url"
      version <- obj .? "version"
      package <- obj .? "package"
      info <- obj .? "info"
      pure $ PursuitSearchResult { text, markup, url, version, package, info }

pursuitRequest :: forall e a. String -> Affjax e Json
pursuitRequest text = affjax $ defaultRequest
  { url = "https://pursuit.purescript.org/search?q=" <> text
  , headers = [ Accept applicationJSON ]
  }

pursuitSearchRequest :: forall eff. String -> Aff ( ajax :: AJAX | eff ) (Array PursuitSearchResult)
pursuitSearchRequest text = do
  res <- pursuitRequest text
  let decoded = decodeJson res.response
  pure $ either (pure []) id $ decoded

pursuitModuleSearchRequest :: forall eff. String -> Aff ( ajax :: AJAX | eff ) (Array PursuitSearchResult)
pursuitModuleSearchRequest text = do
  res <- pursuitRequest text
  let decoded = decodeJson res.response
      results = either (pure []) id $ decoded
  pure $ filter isModule results

  where
    isModule (PursuitSearchResult { info: PursuitSearchInfo { typ: "module" } }) = true
    isModule _ = false

pursuitSearch :: forall eff. Eff (LocalEff eff) Unit
pursuitSearch =
  selectListViewDynamic view (\(PursuitSearchResult { text }) -> log text) Nothing (const "") pursuitSearchRequest 1000
  where
  view (PursuitSearchResult { package, text: txt, info : PursuitSearchInfo { title: Just title, typ, mod: Just mod, typeText } }) = render $
    li ! className "two-lines" $ do
      div ! className "primary-line" $ do
        text title
        case typeText of
          Just tt -> do
            text " :: "
            span ! className "text-info" $ text $ tt
          _ -> text ""
        br
        text txt
      div ! className "secondary-line" $ text $ mod <> " (" <> package <> ")"
  view _ = ""

pursuitSearchModule :: forall eff. LanguageClientConnection -> Eff (LocalEff eff) Unit
pursuitSearchModule conn =
  selectListViewDynamic view importDialog Nothing (const "") pursuitModuleSearchRequest 1000
  where

  view (PursuitSearchResult { package, info: PursuitSearchInfo { mod: Just mod } }) = render $
    li ! className "two-lines" $ do
      div ! className "primary-line" $ text mod
      div ! className "secondary-line" $ text package
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
      li ! className "two-lines" $ do
        div ! className "primary-line" $ do
          text identifier
          text ": "
          span ! className "text-info" $ text typ
        div ! className "secondary-line" $ text mod
