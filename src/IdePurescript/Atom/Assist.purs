module IdePurescript.Atom.Assist where

import Prelude

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
import Control.Monad.Eff.Uncurried (runEffFn2)
import DOM (DOM)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe)
import IdePurescript.Atom.Editor (getActivePosInfo, getLinePosition)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import LanguageServer.IdePurescript.Commands (addClauseCmd, caseSplitCmd, cmdName)
import PscIde (NET)

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

-- TODO: Add to LS
-- type TypoEff e = (net :: NET, note :: NOTIFY, editor :: EDITOR, workspace :: WORKSPACE, dom :: DOM, fs :: FS, ref :: REF, config :: CONFIG, console :: CONSOLE | e)
--
-- fixTypo :: forall eff. Ref State -> Int -> Eff (TypoEff eff) Unit
-- fixTypo modulesState port = do
--   launchAffAndRaise $ runMaybeT body
--   where
--   body :: MaybeT (Aff (TypoEff eff)) Unit
--   body = do
--     atom <- lift $ liftEff'' getAtom
--     ed <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
--     { pos } <- lift $ liftEff'' $ getLinePosition ed
--     { word, range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
--     state <- lift $ liftEff'' $ readRef modulesState
--     corrections <- lift $ eitherToErr (P.suggestTypos port word 2 state.main P.defaultCompletionOptions)
--     liftEff $ selectListViewStatic view (replaceTypo ed wordRange) (Just "identifier") (runCompletion <$> corrections)
--     where
--       runCompletion (TypeInfo obj) = obj
--       replaceTypo ed wordRange { identifier, "module'": mod } =
--         launchAffAndRaise $ do
--          _ <- liftEff $ setTextInBufferRange ed wordRange identifier
--          addIdentImport port modulesState (Just mod) identifier
--       view {identifier, "module'": m} = "<li>" <> m <> "." <> identifier <> "</li>"
--       getIdentFromCompletion (TypeInfo c) = c.identifier
--
-- type GotoEff e = TypoEff e
--
-- -- TODO: Remove, or rewrite (external package?) in terms of definitions api?
-- gotoDef ::  forall eff. Ref State -> Int -> Eff (GotoEff eff) Unit
-- gotoDef modulesState port = do
--   launchAffAndRaise $ runMaybeT body
--   where
--   body :: MaybeT (Aff (GotoEff eff)) Unit
--   body = do
--     atom <- lift $ liftEff'' getAtom
--     ed <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
--     { pos } <- lift $ liftEff'' $ getLinePosition ed
--     { word, range, qualifier } <- MaybeT $ liftEff'' $ getToken ed pos
--     state <- lift $ liftEff'' $ readRef modulesState
--     info <- lift $ getTypeInfo port word state.main qualifier (getUnqualActiveModules state $ Just word) (flip getQualModule $ state)
--     case info of
--       Just (TypeInfo { definedAt : Just (TypePosition { start, end, name }) }) -> lift $ liftEff'' $
--         open atom.workspace name
--           (defaultOpenOptions { initialLine = start.line - 1, initialColumn = start.column - 1 })
--           (const $ pure unit) (pure unit)
--
--       _ -> pure unit
