module IdePurescript.Atom.Editor where

import Prelude

import Atom.Editor (TextEditor, EDITOR, getCursorBufferPosition, getTextInRange)
import Atom.Point (Point, mkPoint, getRow, getColumn)
import Atom.Range (Range, getEnd, getStart, mkRange)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), maybe)
import IdePurescript.Tokens (identifierAtPoint)

getLinePosition :: forall eff. TextEditor -> Eff (editor :: EDITOR | eff) { line :: String, col :: Int, pos :: Point, range :: Range }
getLinePosition ed = do
  pos <- getCursorBufferPosition ed
  let range = mkRange (mkPoint (getRow pos) 0) (mkPoint (getRow pos) 1000)
      col = getColumn pos
  line <- getTextInRange ed range
  pure { line, pos, col, range }

getToken :: forall eff. TextEditor -> Eff (editor :: EDITOR | eff) (Maybe { word :: String, range :: Range, qualifier :: Maybe String })
getToken ed = do
  { line, range, col, pos } <- getLinePosition ed
  line <- getTextInRange ed range
  let pt x = mkPoint (getRow pos) x
      rng {left, right} =  mkRange (pt left) (pt right)
  pure case identifierAtPoint line col of
    Just { word, range, qualifier } -> Just { word, range: rng range, qualifier }
    Nothing -> Nothing
