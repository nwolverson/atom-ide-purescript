module IdePurescript.Atom.PromptPanel where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff
import Data.Maybe
import DOM (DOM)
import DOM.Node.Document (createTextNode, createElement)
import DOM.Node.Element (setClassName, setAttribute)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode, textToNode, elementToEventTarget)
import DOM.HTML.Window (document)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.Event.EventTarget
import DOM.Event.EventTypes
import Atom.Atom
import Atom.Workspace
import Atom.Editor
import Atom.CommandRegistry


foreign import getEditorModel :: forall eff. Element -> Eff (dom :: DOM | eff) TextEditor

foreign import focus :: forall eff. Element -> Eff (dom :: DOM | eff) Unit

addPromptPanel :: forall eff. Aff (dom :: DOM, command :: COMMAND, workspace :: WORKSPACE, editor :: EDITOR | eff) (Maybe String)
addPromptPanel = makeAff $ \err succ ->
 do
  atom <- getAtom
  let w = atom.workspace
      cr = atom.commands
  doc <-  htmlDocumentToDocument <$> (window >>= document)
  div <- createElement "div" doc
  text <- createTextNode "Select Type:" doc
  appendChild (textToNode text) (elementToNode div)
  editor <- createElement "atom-text-editor" doc
  setAttribute "mini" "mini" editor
  appendChild (elementToNode editor) (elementToNode div)
  panel <- addModalPanel w div true 100
  focus editor

  let close :: forall a. Boolean -> a -> Eff _ Unit
      close isSucc _ = do
        model <- getEditorModel editor
        text <- getText model
        destroyPanel panel
        succ (if isSucc then Just text else Nothing)

  addCommand' cr editor "core:confirm" (close true)
  addCommand' cr editor "core:cancel" (close false)

  addEventListener blur (eventListener $ close false) true (elementToEventTarget editor)
