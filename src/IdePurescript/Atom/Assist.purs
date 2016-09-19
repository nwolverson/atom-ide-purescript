module IdePurescript.Atom.Assist (caseSplit, addClause, fixTypo, CaseEff, TypoEff, gotoDef, gotoDefHyper) where

import Prelude
import PscIde as P
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Config (CONFIG)
import Atom.Editor (EDITOR, TextEditor, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Point (Point, getColumn)
import Atom.Range (getEnd, getStart)
import Atom.Workspace (defaultOpenOptions, open, WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT, lift)
import DOM (DOM)
import Data.Foldable (intercalate)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import IdePurescript.Atom.Editor (getLinePosition)
import IdePurescript.Atom.Imports (addIdentImport)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.SelectView (selectListViewStatic)
import IdePurescript.Atom.Tooltips (getToken)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules, State)
import IdePurescript.PscIde (getTypeInfo, eitherToErr)
import Node.FS (FS)
import PscIde (NET)
import PscIde.Command (TypePosition(TypePosition), TypeInfo(..))

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
              | eff)

caseSplit :: forall eff. Int -> Eff (CaseEff eff) Unit
caseSplit port = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (CaseEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    ed :: TextEditor <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { line, col, pos, range } <- lift $ liftEff'' $ getLinePosition ed
    { range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    ty <- MaybeT $ addPromptPanel "Parameter type" ""
    lines <- lift $ eitherToErr $ P.caseSplit port line (getColumn $ getStart wordRange) (getColumn $ getEnd wordRange) false ty
    lift $ void $ liftEff'' $ setTextInBufferRange ed range $ intercalate "\n" lines

addClause :: forall eff. Int -> Eff (CaseEff eff) Unit
addClause port = do
  atom <- getAtom
  editor <- getActiveTextEditor atom.workspace
  case editor of
    Just ed ->
      launchAffAndRaise $ do
        { line, col, range } <- liftEff $ getLinePosition ed
        lines <- eitherToErr $ P.addClause port line false
        liftEff $ setTextInBufferRange ed range $ intercalate "\n" lines
    _ -> pure unit

liftEff'' :: forall e a. Eff e a -> Aff e a
liftEff'' = liftEff

type TypoEff e = (net :: NET, note :: NOTIFY, editor :: EDITOR, workspace :: WORKSPACE, dom :: DOM, fs :: FS, ref :: REF, config :: CONFIG | e)

fixTypo :: forall eff. Ref State -> Int -> Eff (TypoEff eff) Unit
fixTypo modulesState port = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (TypoEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    ed <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { pos } <- lift $ liftEff'' $ getLinePosition ed
    { word, range: wordRange } <- MaybeT $ liftEff'' $ getToken ed pos
    state <- lift $ liftEff'' $ readRef modulesState
    corrections <- lift $ eitherToErr (P.suggestTypos port word 2 state.main)
    liftEff $ selectListViewStatic view (replaceTypo port ed wordRange) Nothing (runCompletion <$> corrections)
    where
      runCompletion (TypeInfo obj) = obj
      replaceTypo port ed wordRange { identifier, "module'": mod } =
        launchAffAndRaise $ do
         liftEff $ setTextInBufferRange ed wordRange identifier
         addIdentImport port modulesState (Just mod) identifier
      view {identifier, "module'": m} = "<li>" <> m <> "." <> identifier <> "</li>"
      getIdentFromCompletion (TypeInfo c) = c.identifier

type GotoEff e = TypoEff (console :: CONSOLE | e)

gotoDef ::  forall eff. Ref State -> Int -> Eff (GotoEff eff) Unit
gotoDef modulesState port = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (GotoEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    ed <- MaybeT $ liftEff'' $ getActiveTextEditor atom.workspace
    { pos } <- lift $ liftEff'' $ getLinePosition ed
    { word, range, qualifier } <- MaybeT $ liftEff'' $ getToken ed pos
    let prefix = maybe "" id qualifier
    state <- lift $ liftEff'' $ readRef modulesState
    info <- lift $ getTypeInfo port word state.main prefix (getUnqualActiveModules state $ Just word) (flip getQualModule $ state)
    case info of
      Just (TypeInfo { definedAt : Just (TypePosition { start, end, name }) }) -> lift $ liftEff'' $
        open atom.workspace name
          (defaultOpenOptions { initialLine = start.line - 1, initialColumn = start.column - 1 })
          (const $ pure unit) (pure unit)

      _ -> pure unit

-- TODO refactor
gotoDefHyper ::  forall eff. Ref State -> Int -> TextEditor -> Point -> Eff (GotoEff eff) Unit
gotoDefHyper modulesState port ed pos = do
  launchAffAndRaise $ runMaybeT body
  where
  body :: MaybeT (Aff (GotoEff eff)) Unit
  body = do
    atom <- lift $ liftEff'' getAtom
    { word, range, qualifier } <- MaybeT $ liftEff'' $ getToken ed pos
    let prefix = maybe "" id qualifier
    state <- lift $ liftEff'' $ readRef modulesState
    info <- lift $ getTypeInfo port word state.main prefix (getUnqualActiveModules state $ Just word) (flip getQualModule $ state)
    case info of
      Just (TypeInfo { definedAt : Just (TypePosition { start, end, name }) }) -> lift $ liftEff'' $
        open atom.workspace name
          (defaultOpenOptions { initialLine = start.line - 1, initialColumn = start.column - 1 })
          (const $ pure unit) (pure unit)

      _ -> pure unit
