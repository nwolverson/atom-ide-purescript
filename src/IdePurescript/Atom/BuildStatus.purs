module IdePurescript.Atom.BuildStatus where

import Prelude
import Control.Monad.Eff

import DOM
import DOM.Node.Document
import DOM.Node.Element
import DOM.Node.Node

import DOM.Node.Types
import DOM.HTML.Document
import DOM.HTML.Window
import DOM.HTML
import DOM.HTML.Types

data BuildStatus = Building | Success | Errors | Failure

instance showBuildStatus :: Show BuildStatus where
  show Building = "Building"
  show Success = "Success"
  show Errors = "Errors"
  show Failure = "Failure"

derive instance eqBuildStatus :: Eq BuildStatus

getBuildStatus :: forall eff. Eff (dom :: DOM | eff) Element
getBuildStatus = do
  doc <-  htmlDocumentToDocument <$> (window >>= document)
  -- docElt <- documentElement doc
  span <- createElement "span" doc
  setClassName (statusIcon Success) span
  text <- createTextNode "PureScript" doc
  appendChild (textToNode text) (elementToNode span)

  pure span

statusIcon :: BuildStatus -> String
statusIcon status = "purescript-build-status icon icon-" ++ case status of
  Success -> "check"
  Errors -> "alert"
  Failure -> "bug"
  Building -> "hourglass"

updateBuildStatus :: forall eff. Element -> BuildStatus -> Eff (dom :: DOM | eff) Unit
updateBuildStatus elt status = do
  setClassName (statusIcon status) elt
