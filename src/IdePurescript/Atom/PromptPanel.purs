module IdePurescript.Atom.PromptPanel where

import Prelude
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand')
import Atom.Editor (setText, EDITOR, TextEditor, getText)
import Atom.Workspace (WORKSPACE, destroyPanel, addModalPanel)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (blur)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode, textToNode, elementToEventTarget)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Nullable (toMaybe, Nullable)

foreign import getEditorModel :: forall eff. Element -> Eff (dom :: DOM | eff) TextEditor

foreign import focus :: forall eff. Element -> Eff (dom :: DOM | eff) Unit

foreign import getActiveElement :: forall eff. Eff (dom :: DOM | eff) (Nullable Element)

addPromptPanel :: forall eff. String -> String -> Aff (dom :: DOM, command :: COMMAND, workspace :: WORKSPACE, editor :: EDITOR | eff) (Maybe String)
addPromptPanel promptText initialText = makeAff $ \err succ ->
 do
  atom <- getAtom
  let w = atom.workspace
      cr = atom.commands
  doc <- htmlDocumentToDocument <$> (window >>= document)
  div <- createElement "div" doc
  text <- createTextNode promptText doc
  _ <- appendChild (textToNode text) (elementToNode div)
  editor <- createElement "atom-text-editor" doc
  setAttribute "mini" "mini" editor
  _ <- appendChild (elementToNode editor) (elementToNode div)
  panel <- addModalPanel w div true 100

  model <- getEditorModel editor
  restext <- setText model initialText

  focussed <- toMaybe <$> getActiveElement
  focus editor

  let close :: forall a. Boolean -> a -> Eff (dom :: DOM, command :: COMMAND, workspace :: WORKSPACE, editor :: EDITOR | eff) Unit
      close isSucc _ = do
        model' <- getEditorModel editor
        restext' <- getText model'
        destroyPanel panel
        maybe (pure unit) focus focussed
        succ (if isSucc then Just restext' else Nothing)

  addCommand' cr editor "core:confirm" (close true)
  addCommand' cr editor "core:cancel" (close false)

  addEventListener blur (eventListener $ close false) true (elementToEventTarget editor)
