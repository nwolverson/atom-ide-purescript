module IdePurescript.Atom.Tooltips where

import Atom.Atom (getAtom)
import Atom.Editor (EDITOR, TextEditor, getTextInRange)
import Atom.Point (Point, getColumn, getRow, mkPoint)
import Atom.Range (mkRange, Range)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Promise (Promise, fromAff)
import Data.Function.Eff (EffFn1, mkEffFn1, runEffFn1)
import Data.Maybe (Maybe(..), maybe)
import Data.String (length, take)
import Data.String.Regex (noFlags, regex)
import IdePurescript.Modules as Modules
import IdePurescript.PscIde (getType)
import IdePurescript.Regex
import Prelude (id, Unit, pure, ($), (>), flip, bind, (<>), (+), unit, void, (-))
import PscIde (NET)

foreign import data TooltipProvider :: *
foreign import mkTooltipProvider :: forall eff a. EffFn1 eff (EffFn1 eff {line :: Int, column :: Int} (Promise a)) TooltipProvider

registerTooltips :: forall eff. Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET, ref:: REF | eff) (Maybe Int) -> Ref (Modules.State) -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET, ref:: REF | eff) Unit
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

getTooltips :: forall eff. Int -> Modules.State -> Point
  -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET | eff) (Promise { valid :: Boolean, info :: String })
getTooltips port state pos = do
  atom <- getAtom
  editor <- getActiveTextEditor atom.workspace
  fromAff $ case editor of
    Just e -> do
      r <- liftEff $ getToken e pos
      case r of
        Just { word, qualifier } -> do
          let prefix = maybe "" id qualifier
          ty <- getType port word state.main prefix (Modules.getUnqualActiveModules state $ Just word) (flip Modules.getQualModule $ state)
          pure { valid: length ty > 0, info: ty }
        _ -> pure { valid: false, info: "" }
    _ -> pure { valid: false, info: "" }
  where
    ignore :: forall a eff'. a -> Eff eff' Unit
    ignore _ = pure unit
