module IdePurescript.Atom.Assist (caseSplit, addClause, fixTypo, CaseEff, TypoEff) where

import Prelude
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Editor (EDITOR, TextEditor, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Point (getColumn)
import Atom.Range (getEnd, getStart)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT, lift)
import DOM (DOM)
import Data.Foldable (intercalate)
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
              | eff)

caseSplit :: forall eff. Eff (CaseEff eff) Unit
caseSplit = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (CaseEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    ed :: TextEditor <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { line, col, pos, range } <- lift $ liftEff'' $ getLinePosition ed
    { range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    ty <- MaybeT $ addPromptPanel "Parameter type" ""
    lines <- lift $ eitherToErr $ P.caseSplit line (getColumn $ getStart wordRange) (getColumn $ getEnd wordRange) true ty
    lift $ void $ liftEff'' $ setTextInBufferRange ed range $ intercalate "\n" lines

addClause :: forall eff. Eff (CaseEff eff) Unit
addClause = do
  atom <- getAtom
  editor <- getActiveTextEditor atom.workspace
  case editor of
    Just ed ->
      launchAffAndRaise $ do
        { line, col, range } <- liftEff $ getLinePosition ed
        lines <- eitherToErr $ P.addClause line true
        liftEff $ setTextInBufferRange ed range $ intercalate "\n" lines
    Nothing -> pure unit

liftEff'' :: forall e a. Eff e a -> Aff e a
liftEff'' = liftEff

type TypoEff e = (net :: NET, note :: NOTIFY, editor :: EDITOR, workspace :: WORKSPACE, dom :: DOM, fs :: FS, ref :: REF | e)
fixTypo :: forall eff. Ref State -> Eff (TypoEff eff) Unit
fixTypo modulesState = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (TypoEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    ed :: TextEditor <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { line, col, pos, range } <- lift $ liftEff'' $ getLinePosition ed
    { word, range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    corrections <- lift $ eitherToErr (P.suggestTypos word 2)
    liftEff $ selectListViewStatic view (replaceTypo ed wordRange) Nothing (runCompletion <$> corrections)
    where
      runCompletion (Completion obj) = obj
      replaceTypo ed wordRange { identifier, "module'": mod } =
        launchAffAndRaise $ do
         liftEff $ setTextInBufferRange ed wordRange identifier
         addIdentImport modulesState (Just mod) identifier
      view {identifier, "module'": m} = "<li>" ++ m ++ "." ++ identifier ++ "</li>"
      getIdentFromCompletion (Completion c) = c.identifier
