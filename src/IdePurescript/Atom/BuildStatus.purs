module IdePurescript.Atom.BuildStatus where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Document (createTextNode, createElement)
import DOM.Node.Element (setClassName)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode, textToNode)
import DOM.HTML.Window (document)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)

data BuildStatus = Building | NotBuilding

instance showBuildStatus :: Show BuildStatus where
  show Building = "Building"
  show NotBuilding = "NotBuilding"

derive instance eqBuildStatus :: Eq BuildStatus

getBuildStatus :: forall eff. Eff (dom :: DOM | eff) Element
getBuildStatus = do
  doc <-  htmlDocumentToDocument <$> (window >>= document)
  span <- createElement "span" doc
  setClassName (statusIcon NotBuilding) span
  text <- createTextNode "PS" doc
  _ <- appendChild (textToNode text) (elementToNode span)

  pure span

statusIcon :: BuildStatus -> String
statusIcon Building = "purescript-build-status icon icon-hourglass"
statusIcon NotBuilding = ""

updateBuildStatus :: forall eff. Element -> BuildStatus -> Eff (dom :: DOM | eff) Unit
updateBuildStatus elt status = do
  setClassName (statusIcon status) elt
