module IdePurescript.Atom.Assist where

import Prelude hiding (div)

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR, getPath)
import Atom.NotificationManager (NOTIFY)
import Atom.Point (Point, getColumn, getRow)
import Atom.Range (Range, getStart)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import Data.Array (fromFoldable, length)
import Data.Either (Either(..))
import Data.Foreign (readArray, toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import IdePurescript.Atom.Editor (getActivePosInfo)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.Search (twoLines)
import IdePurescript.Atom.SelectView (selectListViewStatic)
import IdePurescript.Atom.Util (launchAffAndRaise)
import LanguageServer.IdePurescript.Assist (TypoResult(..), decodeTypoResult)
import LanguageServer.IdePurescript.Commands (addClauseCmd, caseSplitCmd, cmdName, fixTypoCmd)
import LanguageServer.Types (DocumentUri)
import LanguageServer.Uri (filenameToUri)
import PscIde (NET)
import Text.Smolder.Markup (text)
import Text.Smolder.Renderer.String (render)

type CaseEff eff =
              (dom :: DOM
              , command :: COMMAND
              , workspace :: WORKSPACE
              , editor :: EDITOR
              , net :: NET
              , note :: NOTIFY
              , config :: CONFIG
              , console :: CONSOLE
              , exception :: EXCEPTION
              | eff)

caseSplit :: forall eff. LanguageClientConnection -> Eff (CaseEff eff) Unit
caseSplit conn =
  getActivePosInfo >>= maybe (pure unit) \{ pos, uri } -> launchAffAndRaise do
    addPromptPanel "Parameter type" "" >>= maybe (pure unit) \typ ->
      unit <$ executeCommand conn
        { command: cmdName caseSplitCmd
        , arguments: [ toForeign uri, toForeign $ getRow pos, toForeign $ getColumn pos, toForeign typ ]
        }

addClause :: forall eff. LanguageClientConnection -> Eff (CaseEff eff) Unit
addClause conn = do
  getActivePosInfo >>= maybe (pure unit) \{ pos, uri } -> launchAffAndRaise $
    unit <$ executeCommand conn
      { command: cmdName addClauseCmd
      , arguments: [ toForeign uri, toForeign $ getRow pos, toForeign $ getColumn pos ]
      }

fixTypoWithRange :: forall eff. LanguageClientConnection -> Range -> Eff (CaseEff eff) Unit
fixTypoWithRange conn range = void $ runMaybeT do
  atom <- liftEff  getAtom
  ed <- MaybeT $ getActiveTextEditor atom.workspace
  path <- MaybeT $ getPath ed
  url <- lift $ filenameToUri path
  lift $ launchAffAndRaise $ fixTypoWithPos conn (getStart range) url

fixTypo :: forall eff. LanguageClientConnection -> Eff (CaseEff eff) Unit
fixTypo conn = do 
  getActivePosInfo >>= maybe (pure unit) \{ pos, uri } -> launchAffAndRaise $ 
    fixTypoWithPos conn pos uri

fixTypoWithPos :: forall eff. LanguageClientConnection -> Point -> DocumentUri -> Aff (CaseEff eff) Unit
fixTypoWithPos conn pos uri = go Nothing
  where
  go choice = do
    res <- executeCommand conn
      { command: cmdName fixTypoCmd
      , arguments: [ toForeign uri, toForeign $ getRow pos, toForeign $ getColumn pos ] <> fromFoldable choice
      }
    case runExcept $ readArray res >>= traverse decodeTypoResult of
      Right arr | length arr > 0 ->
        liftEff $ selectListViewStatic view (launchAffAndRaise <<< go <<< Just <<< toForeign) (Just "identifier") arr
      _ -> pure unit

  view :: TypoResult -> String
  view (TypoResult { identifier, mod }) = render $
    twoLines (text identifier) (text mod)
