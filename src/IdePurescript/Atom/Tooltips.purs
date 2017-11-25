module IdePurescript.Atom.Tooltips where

import Prelude

import Atom.Editor (EDITOR, TextEditor, getTextInRange)
import Atom.Point (Point, getColumn, getRow, mkPoint)
import Atom.Range (mkRange, Range)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length, take)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.Regex (match')

-- TODO use identifierAtPoint
getToken :: forall eff. TextEditor -> Point -> Eff (editor :: EDITOR | eff) (Maybe { word :: String, range :: Range, qualifier :: Maybe String })
getToken e pos = do
  let row = getRow pos
      col = getColumn pos
      beforePos = mkPoint row 0
      afterPos = mkPoint row (col + 100)
      beforeRegex = regex "[a-zA-Z_0-9']*$" noFlags
      afterRegex = regex "^[a-zA-Z_0-9']*" noFlags
      moduleRegex = regex """(?:^|[^A-Za-z_.])(?:((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.)$""" noFlags

  textBefore <- getTextInRange e (mkRange beforePos pos)
  textAfter <- getTextInRange e (mkRange pos afterPos)
  let wordRange left right = mkRange (mkPoint row (col - left)) (mkPoint row (col + right))
  pure $ case { before: match' beforeRegex textBefore, after: match' afterRegex textAfter } of
              { before: Just [Just s], after: Just [Just s'] }
                ->
                  let qualifier = case match' moduleRegex (take (length textBefore - length s) textBefore) of
                                      Just [ _, mm ] -> mm
                                      _ -> Nothing
                  in
                    Just { word : s<>s', range : wordRange (length s) (length s'), qualifier }
              _ -> Nothing
