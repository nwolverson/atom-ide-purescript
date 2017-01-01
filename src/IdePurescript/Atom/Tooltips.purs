module IdePurescript.Atom.Tooltips where

import Prelude
import IdePurescript.Modules as Modules
import Atom.Atom (getAtom)
import Atom.Editor (EDITOR, TextEditor, getTextInRange)
import Atom.NotificationManager (NOTIFY)
import Atom.Point (Point, getColumn, getRow, mkPoint)
import Atom.Range (mkRange, Range)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Promise (Promise, fromAff)
import DOM (DOM)
import Data.Function.Eff (EffFn1, EffFn2, mkEffFn1, runEffFn1, runEffFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length, take)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.Atom.Editor (getLinePosition)
import IdePurescript.Atom.Util (launchAffAndRaise)
import IdePurescript.PscIde (getType)
import IdePurescript.Regex (match')
import PscIde (NET)

foreign import data TooltipProvider :: *

foreign import mkTooltipProvider :: forall eff a. EffFn1 eff (EffFn1 eff {line :: Int, column :: Int} (Promise a)) TooltipProvider

foreign import showTooltip :: forall eff. EffFn2 (dom :: DOM, workspace :: WORKSPACE, editor :: EDITOR | eff) Point String Unit

registerTooltips :: forall eff. Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET, ref:: REF | eff) (Maybe Int) -> Ref (Modules.State)
  -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET, ref:: REF | eff) Unit
registerTooltips getPort ref = do
  void $ runEffFn1 mkTooltipProvider (mkEffFn1 \{line,column} -> do
    state <- readRef ref
    let point = mkPoint (line-1) (column-1)
    port <- getPort
    maybe (fromAff $ pure { valid: false, info: "" }) (\p -> getTooltips p state point) port
  )

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

constructTooltip :: forall eff. Int -> Modules.State -> Point
  -> Aff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET | eff) (Maybe String)
constructTooltip port state pos = do
  atom <- liftEff $ getAtom
  editor <- liftEff $ getActiveTextEditor atom.workspace
  case editor of
    Just e -> liftEff (getToken e pos) >>= case _ of
        Just { word, qualifier } -> do
          ty <- getType port word state.main qualifier (Modules.getUnqualActiveModules state $ Just word) (flip Modules.getQualModule $ state)
          pure $ if length ty > 0 then Just ty else Nothing
        _ -> pure Nothing
    _ -> pure Nothing

getTooltips :: forall eff. Int -> Modules.State -> Point
  -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET | eff) (Promise { valid :: Boolean, info :: String })
getTooltips port state pos = fromAff $ do
  text <- constructTooltip port state pos
  pure $ maybe { valid: false, info: "" } { valid: true, info: _} text

showTooltipAtCursor :: forall eff. Int -> Ref Modules.State -> Eff (console :: CONSOLE, note :: NOTIFY, workspace :: WORKSPACE, editor :: EDITOR, dom :: DOM, net :: NET, ref :: REF | eff) Unit
showTooltipAtCursor port modulesState = launchAffAndRaise do
  atom <- liftEff getAtom
  editor <- liftEff $ getActiveTextEditor atom.workspace
  state <- liftEff $ readRef modulesState
  case editor of
    Just ed -> do
      { pos } <- liftEff $ getLinePosition ed
      text <- constructTooltip port state pos
      liftEff $ maybe (pure unit) (runEffFn2 showTooltip pos) text
    Nothing -> pure unit
