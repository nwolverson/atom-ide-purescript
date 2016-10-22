module IdePurescript.Atom.QuickFixes (showQuickFixes) where

import Prelude
import Data.String as Str
import Data.String.Regex (regex) as Regex
import Data.String.Regex.Flags (global, noFlags) as Regex
import Atom.Config (CONFIG)
import Atom.Editor (getTextInRange, EDITOR, TextEditor, getCursorBufferPosition, setTextInBufferRange)
import Atom.NotificationManager (NOTIFY)
import Atom.Point (getColumn, getRow, mkPoint)
import Atom.Range (getEnd, getStart, mkRange, containsPoint)
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
import IdePurescript.QuickFix (isUnknownToken, getTitle)
import IdePurescript.Regex (replace', test')
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
  , config :: CONFIG
  , fs :: FS
  , ref :: REF | a)

showQuickFixes :: forall eff. Int -> Ref State -> TextEditor -> LinterInternal -> Array AtomLintMessage -> Eff (QuickFixEff eff) Unit
showQuickFixes port modulesState editor linterMain messages = do
  pos <- getCursorBufferPosition editor
  editorLinter <- getEditorLinter linterMain editor
  messages <- getMessages editorLinter
  messages' <- filterM (inRange editorLinter pos) messages
  fixes <- catMaybes <$> traverse (getFix editorLinter) messages'
  selectListViewStaticInline view applyFix Nothing fixes
  pure unit

  where
    inRange editorLinter point message = do
      range <- toMaybe <$> getMarkerBufferRange editorLinter message
      pure $ maybe false (\r -> containsPoint r point) range

    getFix editorLinter message@{suggestion : { hasSuggestion: true, replacement, range }, errorCode } = do
      markerRange <- toMaybe <$> getMarkerBufferRange editorLinter message
      let markerRow = maybe 0 (getRow <<< getStart) markerRange -- must always be set

      pure $ case range of
        [ [r, c], [r', c'] ] ->
          let
            -- Always use replacement range from suggestion, but adjust to line of marker as marker will track buffer edits
            -- TODO: Create markers for replacement ranges or wait until linter supports this
            rowDiff = markerRow - r
            newRange = mkRange (mkPoint (r+rowDiff) c) (mkPoint (r'+rowDiff) c')
          in getFix' newRange
        _ -> Nothing
      where
        getFix' range = Just
          { title: getTitle errorCode
          , action: do
              let trailingNewline = test' (Regex.regex "\n\\s+$" Regex.noFlags) replacement
              extraText <- getTextInRange editor (mkRange (getEnd range) (mkPoint (getRow $ getEnd range) (10 + (getColumn $ getEnd range))))
              let addNewline = trailingNewline && (not $ Str.null extraText)
              let replacement' = Str.trim $ replace' (Regex.regex "\\s+\n" Regex.global) "\n" replacement
              setTextInBufferRange editor range (replacement' <> if addNewline then "\n" else "")
              log $ "Applied fix: " <> errorCode
          }

    getFix _ { errorCode } | isUnknownToken errorCode
      = pure $ Just { title: "Fix typo", action: fixTypo modulesState port }

    getFix _ _ = pure Nothing

    view { title } = "<li>" <> title <> "</li>"
    applyFix { action } = action
