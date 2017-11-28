module IdePurescript.Atom.Assist where

import Prelude hiding (div)

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR, TextEditor, getPath, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Point (Point, getColumn, getRow)
import Atom.Workspace (defaultOpenOptions, open, WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (fromFoldable, length)
import Data.Either (Either(..))
import Data.Foreign (readArray, readString, toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import IdePurescript.Atom.Editor (getActivePosInfo, getLinePosition)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.Search (twoLines)
import IdePurescript.Atom.SelectView (selectListViewStatic)
import LanguageServer.IdePurescript.Assist (TypoResult(..), decodeTypoResult)
import LanguageServer.IdePurescript.Commands (addClauseCmd, caseSplitCmd, cmdName, fixTypoCmd)
import PscIde (NET)
import Text.Smolder.HTML (div, li)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.String (render)

launchAffAndRaise :: forall a e. Aff (note :: NOTIFY | e) a -> Eff (note :: NOTIFY | e) Unit
launchAffAndRaise = void <<< (runAff raiseError (const $ pure unit))
  where
  raiseError :: forall eff. Error -> Eff (note :: NOTIFY | eff) Unit
  raiseError e = do
    atom <- getAtom
    addError atom.notifications (show e)

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

fixTypo :: forall eff. LanguageClientConnection -> Eff (CaseEff eff) Unit
fixTypo conn = do
  getActivePosInfo >>= maybe (pure unit) \{ pos, uri } -> launchAffAndRaise $ go pos uri Nothing

  where
    go pos uri choice = do
      res <- executeCommand conn
        { command: cmdName fixTypoCmd
        , arguments: [ toForeign uri, toForeign $ getRow pos, toForeign $ getColumn pos ] <> fromFoldable choice
        }
      case runExcept $ readArray res >>= traverse decodeTypoResult of
        Right arr | length arr > 0 ->
          liftEff $ selectListViewStatic view (launchAffAndRaise <<< go pos uri <<< Just <<< toForeign) (Just "identifier") arr
        _ -> pure unit

    view :: TypoResult -> String
    view (TypoResult { identifier, mod }) = render $
      twoLines (text identifier) (text mod)
