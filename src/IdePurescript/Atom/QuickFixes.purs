module IdePurescript.Atom.QuickFixes (showQuickFixes) where

import Prelude
import Atom.Editor (EDITOR, TextEditor, getCursorBufferPosition, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY)
import Atom.Range (containsPoint)
import Atom.Workspace (WORKSPACE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref)
import DOM (DOM)
import Data.Array (catMaybes, filterM)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import IdePurescript.Atom.Assist (fixTypo)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.Hooks.Linter (LINTER, getMarkerBufferRange, getMessages, getEditorLinter, LinterInternal)
import IdePurescript.Atom.SelectView (selectListViewStaticInline)
import IdePurescript.Modules (State)
import Node.FS (FS)
import PscIde (NET)

type QuickFixEff a =
  ( editor :: EDITOR
  , linter :: LINTER
  , dom :: DOM
  , console :: CONSOLE
  , net :: NET
  , note :: NOTIFY
  , workspace :: WORKSPACE
  , fs :: FS
  , ref :: REF | a)

showQuickFixes :: forall eff. Ref State -> TextEditor -> LinterInternal -> Array AtomLintMessage -> Eff (QuickFixEff eff) Unit
showQuickFixes modulesState editor linterMain messages = do
  pos <- getCursorBufferPosition editor
  editorLinter <- getEditorLinter linterMain editor
  messages <- getMessages editorLinter
  messages' <- filterM (inRange editorLinter pos) messages
  fixes <- catMaybes <$> traverse (getFix editorLinter) messages'
  selectListViewStaticInline view applyFix Nothing fixes

  where
    inRange editorLinter point message = do
      range <- toMaybe <$> getMarkerBufferRange editorLinter message
      pure $ maybe false (\r -> containsPoint r point) range

    getFix editorLinter message@{suggestion : { hasSuggestion: true, replacement }, errorCode } = do
      range <- toMaybe <$> getMarkerBufferRange editorLinter message
      pure $ maybe Nothing getFix' range
      where
        getFix' range = Just
          { title: getTitle errorCode
          , action: do
              setTextInBufferRange editor range replacement
              log $ "Applied fix: " ++ errorCode
          }

        getTitle "UnusedImport"                = "Remove import"
        getTitle "RedundantEmptyHidingImport"  = "Remove import"
        getTitle "DuplicateImport"             = "Remove import"
        getTitle "RedundantUnqualifiedImport"  = "Remove import"
        getTitle "DeprecatedQualifiedSyntax"   = "Remove qualified keyword"
        getTitle "ImplicitImport"              = "Make import explicit"
        getTitle "UnusedExplicitImport"        = "Remove unused references"
        getTitle _                             = "Apply Suggestion"
    getFix _ { errorCode } |
      errorCode == "UnknownValue" ||
      errorCode == "UnknownType" ||
      errorCode == "UnknownDataConstructor" ||
      errorCode == "UnknownTypeConstructor"
      = pure $ Just { title: "Fix typo", action: fixTypo modulesState }

    getFix _ _ = pure Nothing

    view { title } = "<li>" ++ title ++ "</li>"
    applyFix { action } = action
