module IdePurescript.Atom.Editor where

import Prelude
import Atom.Editor (TextEditor, EDITOR, getCursorBufferPosition, getTextInRange)
import Atom.Point (Point, mkPoint, getRow, getColumn)
import Atom.Range (Range, mkRange)
import Control.Monad.Eff (Eff)

getLinePosition :: forall eff. TextEditor -> Eff (editor :: EDITOR | eff) { line :: String, col :: Int, pos :: Point, range :: Range }
getLinePosition ed = do
  pos <- getCursorBufferPosition ed
  let range = mkRange (mkPoint (getRow pos) 0) (mkPoint (getRow pos) 1000)
      col = getColumn pos
  line <- getTextInRange ed range
  pure { line, pos, col, range }
