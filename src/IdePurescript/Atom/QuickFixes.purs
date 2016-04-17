module IdePurescript.Atom.QuickFixes (showQuickFixes) where

import Prelude
import Atom.Editor (TextEditor, EDITOR, getCursorBufferPosition, setTextInBufferRange)
import Atom.Range (containsPoint)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (catMaybes, filterM)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.Hooks.Linter (LINTER, getMarkerBufferRange, getMessages, getEditorLinter, LinterInternal)
import IdePurescript.Atom.SelectView (selectListViewStaticInline)

type QuickFixEff a = (linter :: LINTER, editor :: EDITOR, console :: CONSOLE, dom :: DOM | a)

showQuickFixes :: forall eff. TextEditor -> LinterInternal -> Array AtomLintMessage -> Eff (QuickFixEff eff) Unit
showQuickFixes editor linterMain messages = do
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

    getFix _ _ = pure Nothing

    view { title } = "<li>" ++ title ++ "</li>"
    applyFix { action } = action
