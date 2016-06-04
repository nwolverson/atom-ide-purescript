module IdePurescript.Atom.Assist (caseSplit, addClause, fixTypo, CaseEff, TypoEff) where

import Prelude
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (EDITOR, TextEditor, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Point (getColumn)
import Atom.Range (getEnd, getStart)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT, lift)
import DOM (DOM)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Foreign (readInt)
import Data.Maybe (Maybe(Nothing, Just))
import IdePurescript.Atom.Editor (getLinePosition)
import IdePurescript.Atom.Imports (addIdentImport)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.SelectView (selectListViewStatic)
import IdePurescript.Atom.Tooltips (getToken)
import IdePurescript.Modules (State)
import IdePurescript.PscIde (eitherToErr)
import Node.FS (FS)
import PscIde (NET)
import PscIde.Command (Completion(..))


-- TODO
launchAffAndRaise :: forall a e. Aff (note :: NOTIFY | e) a -> Eff (note :: NOTIFY | e) Unit
launchAffAndRaise = runAff raiseError (const $ pure unit)
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
              | eff)

getPort :: forall eff. Eff (config :: CONFIG | eff) (Maybe Int)
getPort = do
  atom <- getAtom
  port <- readInt <$> getConfig atom.config "ide-purescript.pscIdePort"
  pure $ either (const Nothing) Just port

caseSplit :: forall eff. Eff (CaseEff eff) Unit
caseSplit = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (CaseEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    port <- MaybeT $ liftEff'' getPort
    ed :: TextEditor <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { line, col, pos, range } <- lift $ liftEff'' $ getLinePosition ed
    { range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    ty <- MaybeT $ addPromptPanel "Parameter type" ""
    lines <- lift $ eitherToErr $ P.caseSplit port line (getColumn $ getStart wordRange) (getColumn $ getEnd wordRange) true ty
    lift $ void $ liftEff'' $ setTextInBufferRange ed range $ intercalate "\n" lines

addClause :: forall eff. Eff (CaseEff eff) Unit
addClause = do
  atom <- getAtom
  editor <- getActiveTextEditor atom.workspace
  portRaw <- getPort
  case editor, portRaw of
    Just ed, Just port ->
      launchAffAndRaise $ do
        { line, col, range } <- liftEff $ getLinePosition ed
        lines <- eitherToErr $ P.addClause port line true
        liftEff $ setTextInBufferRange ed range $ intercalate "\n" lines
    _, _ -> pure unit

liftEff'' :: forall e a. Eff e a -> Aff e a
liftEff'' = liftEff

type TypoEff e = (net :: NET, note :: NOTIFY, editor :: EDITOR, workspace :: WORKSPACE, dom :: DOM, fs :: FS, ref :: REF, config :: CONFIG | e)

fixTypo :: forall eff. Ref State -> Eff (TypoEff eff) Unit
fixTypo modulesState = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (TypoEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    port <- MaybeT $ liftEff'' getPort
    ed :: TextEditor <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { line, col, pos, range } <- lift $ liftEff'' $ getLinePosition ed
    { word, range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    state <- lift $ liftEff'' $ readRef modulesState
    corrections <- lift $ eitherToErr (P.suggestTypos port word 2 state.main)
    liftEff $ selectListViewStatic view (replaceTypo port ed wordRange) Nothing (runCompletion <$> corrections)
    where
      runCompletion (Completion obj) = obj
      replaceTypo port ed wordRange { identifier, "module'": mod } =
        launchAffAndRaise $ do
         liftEff $ setTextInBufferRange ed wordRange identifier
         addIdentImport port modulesState (Just mod) identifier
      view {identifier, "module'": m} = "<li>" ++ m ++ "." ++ identifier ++ "</li>"
      getIdentFromCompletion (Completion c) = c.identifier
