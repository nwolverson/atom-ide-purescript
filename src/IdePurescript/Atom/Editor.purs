module IdePurescript.Atom.Editor where

import Prelude

import Atom.Atom (getAtom)
import Atom.Editor (EDITOR, TextEditor, getCursorBufferPosition, getPath, getTextInRange)
import Atom.Point (Point, mkPoint, getRow, getColumn)
import Atom.Range (Range, getEnd, getStart, mkRange)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..), maybe)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.Types (DocumentUri)
import LanguageServer.Uri (filenameToUri)

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

getActivePosInfo :: forall eff. Eff ( workspace :: WORKSPACE, editor :: EDITOR, exception :: EXCEPTION | eff)
  (Maybe { line :: String, pos :: Point, ed :: TextEditor, uri :: DocumentUri })
getActivePosInfo = do
  atom <- getAtom
  getActiveTextEditor atom.workspace >>= maybe (pure Nothing) \ed -> do
    {line,col,pos,range} <- getLinePosition ed
    getPath ed >>= maybe (pure Nothing) \path -> do
      uri <- filenameToUri path
      pure $ Just { line, pos, ed, uri }
