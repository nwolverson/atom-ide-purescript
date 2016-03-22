module IdePurescript.Atom.Tooltips where

import Prelude (Unit, pure, ($), (>), flip, bind, (++), (+), unit, void, (-))
import Data.String.Regex (match, noFlags, regex)
import Data.String (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Function.Eff (EffFn1, mkEffFn1, runEffFn1)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Promise (Promise, fromAff)

import Atom.Atom (getAtom)
import Atom.Point (Point, getColumn, getRow, mkPoint)
import Atom.Range (mkRange, Range)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Atom.Editor (EDITOR, TextEditor, getTextInRange)
import IdePurescript.PscIde (getType)
import IdePurescript.Modules as Modules
import PscIde (NET)

foreign import data TooltipProvider :: *
foreign import mkTooltipProvider :: forall eff a. EffFn1 eff (EffFn1 eff Point (Promise a)) TooltipProvider

registerTooltips :: forall eff. Ref (Modules.State) -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET, ref:: REF | eff) Unit
registerTooltips ref = do
  void $ runEffFn1 mkTooltipProvider (mkEffFn1 \pos -> do
    state <- readRef ref
    getTooltips state pos)

getToken :: forall eff. TextEditor -> Point -> Eff (editor :: EDITOR | eff) (Maybe { word :: String, range :: Range })
getToken e pos = do
  let row = getRow pos
      col = getColumn pos
      beforePos = mkPoint row 0
      afterPos = mkPoint row (col + 100)
      beforeRegex = regex "[a-zA-Z_0-9']*$" noFlags
      afterRegex = regex "^[a-zA-Z_0-9']*" noFlags
  textBefore <- getTextInRange e (mkRange beforePos pos)
  textAfter <- getTextInRange e (mkRange pos afterPos)
  let wordRange left right = mkRange (mkPoint row (col - left)) (mkPoint row (col + right))
  pure $ case { before: match beforeRegex textBefore, after: match afterRegex textAfter } of
              { before: Just [Just s], after: Just [Just s'] }
                -> Just { word : s++s', range : wordRange (length s) (length s') }
              _ -> Nothing

getTooltips :: forall eff. Modules.State -> Point
  -> Eff (workspace :: WORKSPACE, editor :: EDITOR, net :: NET | eff) (Promise { valid :: Boolean, info :: String })
getTooltips state pos = do
  atom <- getAtom
  editor <- getActiveTextEditor atom.workspace
  case editor of
    Just e -> do
      word' <- getToken e pos
      let word = maybe "" _.word word'
      -- let prefix = case match (regex "^(.*)\." noFlags) word of
      --   Just [_, Just p] -> p
      --   _ -> ""
      let prefix = ""
        --text modulePrefix unqualModules getQualifiedModule
        --String -> String -> Array String -> (String -> Array String)
      fromAff do
        ty <- getType word prefix (Modules.getUnqualActiveModules state) (flip Modules.getQualModule $ state)
        pure { valid: length ty > 0, info: ty }
    _ -> fromAff $ pure { valid: false, info: "" }
  where
    ignore :: forall a eff'. a -> Eff eff' Unit
    ignore _ = pure unit


-- ooltips extends HoverTooltips
-- #   constructor: (@pscIde) ->
-- #     super()
-- #     @syntax = 'source.purescript'
-- #     @provider = (pos) => new Promise (resolve) =>
-- #       p = [pos.line-1, pos.column-1]
-- #       editor = atom.workspace.getActivePaneItem()
-- #       buffer = editor.buffer
-- #
-- #       regex = /[a-zA-Z_0-9']*/
-- #       match = ""
-- #       initialLength = 0
-- #       buffer.backwardsScanInRange(regex, [[p[0], 0], p], (it) ->
-- #         match = it.matchText
-- #         initialLength = match.length
-- #       )
-- #       buffer.scanInRange(regex, [p, [p[0], Infinity]], (it) ->
-- #         match += it.matchText
-- #       )
-- #
-- #       prefix = getModulePrefix(editor, Point.fromObject(p).translate([0, -initialLength]))
-- #
-- #       @pscIde.getType(match, prefix)
-- #         .then (result) =>
-- #           resolve { valid: result.length > 0, info: result }
-- #         .catch (err) =>
-- #           resolve { valid: false }
-- #
