module IdePurescript.Atom.Psci where

import Prelude

import Ansi.Codes (Color(..))
import Atom.Atom (getAtom)
import Atom.CommandRegistry (addCommand', COMMAND, addCommand)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (setText, getText, TextEditor, EDITOR, getSelectedText, moveToBeginningOfLine, moveDown, getTextInRange, getCursorBufferPosition)
import Atom.Grammar (GRAMMAR)
import Atom.GrammarRegistry (grammarForScopeName)
import Atom.NotificationManager (addError, NOTIFY)
import Atom.Pane (PANE, destroyItem)
import Atom.Point (getRow, mkPoint)
import Atom.Project (PROJECT, getPaths)
import Atom.Range (mkRange)
import Atom.Workspace (getActiveTextEditor, WORKSPACE, defaultOpenOptions, open, addOpener, paneForItem)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (whileE, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (catchException, error)
import Control.Monad.Eff.Ref (readRef, writeRef, Ref, newRef, REF)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (scrollHeight, setAttribute, setClassName, setScrollTop)
import DOM.Node.Node (setTextContent, firstChild, removeChild, hasChildNodes, appendChild)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (elementToParentNode, elementToNode, Element)
import DOM.Util (setTimeout)
import Data.Array (catMaybes, cons, drop, uncons, (!!))
import Data.Either (either, Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (readString)
import Data.Int (fromNumber)
import Data.Maybe (maybe, Maybe(Nothing, Just), isJust, fromMaybe)
import Data.Nullable (toNullable, Nullable, toMaybe)
import Data.String (Pattern(Pattern), indexOf, trim)
import Data.String.Regex (split, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Global (readInt)
import IdePurescript.Atom.PromptPanel (focus)
import IdePurescript.Atom.Util (launchAffAndRaise)
import IdePurescript.Regex (match')
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(Normally), defaultSpawnOptions, onClose, onError, spawn, stderr, stdin, stdout, toStandardError)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Stream (end, writeString, onDataString)
import PscIde.Project (getRoot)
import Unsafe.Coerce (unsafeCoerce)

type PscPane = { getTitle :: Unit -> String, element :: Element }

createElement' :: forall eff. String -> Eff (dom :: DOM | eff) Element
createElement' elt = do
  doc <- htmlDocumentToDocument <$> (window >>= document)
  createElement elt doc

foreign import getModel :: forall eff. Element -> Eff (dom :: DOM | eff) (Nullable TextEditor)

foreign import copy :: forall eff. Eff (dom :: DOM | eff) Unit

paneUri :: String
paneUri = "purescript://psci"

registerCommands :: forall eff. Eff (command :: COMMAND, dom :: DOM | eff) Unit
registerCommands = do
  atom <- getAtom
  addCommand atom.commands ".psci-pane" "ide-purescript:psci-copy" $ const copy

opener :: forall eff. String -> Eff (dom :: DOM, console :: CONSOLE | eff) (Nullable PscPane)
opener s =
  case indexOf (Pattern paneUri) s of
    Just 0 -> do
      div <- createElement' "div"
      setClassName "psci-pane" div
      setAttribute "tabindex" "0" div
      topDiv <- createElement' "div"
      setClassName "psci-lines" topDiv
      _ <- appendChild (elementToNode topDiv) (elementToNode div)
      bottomDiv <- createElement' "div"
      setClassName "psci-input padded inset-panel" bottomDiv

      editorElt <- createElement' "atom-text-editor"
      setAttribute "mini" "true" editorElt
      _ <- appendChild (elementToNode editorElt) (elementToNode bottomDiv)

      _ <- appendChild (elementToNode bottomDiv) (elementToNode div)

      setTimeout 10 $ focus editorElt

      pure $ toNullable $ Just { getTitle: \_ -> "PSCI", element: div }
    _ -> pure $ toNullable Nothing

openPsci :: forall eff. Aff (ref :: REF, console :: CONSOLE, workspace :: WORKSPACE, note :: NOTIFY, dom :: DOM | eff) TextEditor
openPsci = makeAff \err cb -> do
  atom <- getAtom
  _ <- addOpener atom.workspace (unsafeCoerce opener)
  open atom.workspace paneUri (defaultOpenOptions { split = "right", activatePane = false }) cb (err $ error "Can't Open PSCI")

type PsciEff eff =
  ( ref :: REF
  , console :: CONSOLE
  , workspace :: WORKSPACE
  , pane :: PANE
  , note :: NOTIFY
  , grammar :: GRAMMAR
  , editor :: EDITOR
  , config :: CONFIG
  , fs :: FS
  , project :: PROJECT
  , cp :: CHILD_PROCESS
  , dom :: DOM
  , command :: COMMAND | eff)

-- TODO: contribute to purescript-ansi
colorForCode :: Int -> Maybe Color
colorForCode c =
  case c of
    30 -> Just Black
    31 -> Just Red
    32 -> Just Green
    33 -> Just Yellow
    34 -> Just Blue
    35 -> Just Magenta
    36 -> Just Cyan
    37 -> Just White
    90 -> Just Grey
    91 -> Just BrightRed
    92 -> Just BrightGreen
    93 -> Just BrightYellow
    94 -> Just BrightBlue
    95 -> Just BrightMagenta
    96 -> Just BrightCyan
    97 -> Just BrightWhite
    _  -> Nothing

data Rgb = Rgb Int Int Int

xtermColor :: Color -> Rgb
xtermColor c = case c of
  White         -> Rgb 229 229 229
  Black         -> Rgb 0 0 0
  Blue          -> Rgb 0 0 238
  Cyan          -> Rgb 0 205 205
  Green         -> Rgb 0 205 0
  Magenta       -> Rgb 205 0 205
  Red           -> Rgb 205 0 0
  Yellow        -> Rgb 205 205 0
  Grey          -> Rgb 127 127 127
  BrightBlack   -> Rgb 127 127 127
  BrightRed     -> Rgb 255 0 0
  BrightGreen   -> Rgb 0 255 0
  BrightYellow  -> Rgb 255 255 0
  BrightBlue    -> Rgb 92 92 255
  BrightMagenta -> Rgb 255 0 255
  BrightCyan    -> Rgb 0 255 255
  BrightWhite   -> Rgb 255 255 255

renderRgbCss :: Rgb -> String
renderRgbCss (Rgb r g b) = "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"

replaceAnsiColor :: forall eff. String -> Eff (PsciEff eff) (Array Element)
replaceAnsiColor text = toNodes parts
  where
    parts :: Array String
    parts = either (const []) (\r -> split r text) (regex """(\x1b\[[0-9;]+m)""" noFlags)

    colEscape :: String -> Maybe Int
    colEscape str = case match' (regex """\x1b\[([0-9]+)m""" noFlags) str of
      Just [_, Just num] -> fromNumber (readInt 10 num)
      _ -> Nothing

    colorCss :: Int -> String
    colorCss n = "color: " <> maybe "black" (renderRgbCss <<< xtermColor) (colorForCode n)

    toNodes :: Array String -> Eff (PsciEff eff) (Array Element)
    toNodes [] = pure []
    toNodes xs | (maybe Nothing colEscape (xs !! 2)) == Just 0 && isJust (maybe Nothing colEscape (xs !! 0)) =
      case maybe Nothing colEscape (xs !! 0), xs !! 1 of
        Just n, Just innerText -> do
          span <- createElement' "span"
          setTextContent innerText (elementToNode span)
          setAttribute "style" (colorCss n) span
          rest <- toNodes (drop 3 xs)
          pure $ cons span rest
        _, _ -> pure []
    toNodes xs = do
      let str = fromMaybe "" (xs !! 0)
      span <- createElement' "span"
      setTextContent str (elementToNode span)
      rest <- toNodes (drop 1 xs)
      pure $ cons span rest

appendText :: forall eff. PscPane  -> String -> Eff (PsciEff eff) Unit
appendText {element} text = do
  div <- createElement' "div"
  setClassName "psci-line" div
  replaceAnsiColor text >>= traverse_ \node -> appendChild (elementToNode node) (elementToNode div)
  lines <- querySelector (QuerySelector ".psci-lines") (elementToParentNode element)
  maybe (log "appendText failed") (\lines' -> do
    _ <- appendChild (elementToNode div) (elementToNode lines')
    height <- scrollHeight lines'
    setScrollTop height lines') lines

clearText :: forall eff. PscPane -> Eff (PsciEff eff) Unit
clearText {element} = do
  let node = elementToNode element
  whileE (hasChildNodes node) do
    child <- firstChild node
    maybe (pure unit) (void <<< flip removeChild node) child

sendText :: forall eff. PscPane -> ChildProcess -> String -> Eff (PsciEff eff) Unit
sendText pane proc text = do
  let text' = trim text <> "\n"
  appendText pane text'
  void $ writeString (stdin proc) UTF8 text' (pure unit)

sendText' :: forall eff. (TextEditor -> Eff (PsciEff eff) String) -> PscPane -> ChildProcess -> Eff (PsciEff eff) Unit
sendText' getText pane proc  = do
  atom <- getAtom
  activeEditor <- getActiveTextEditor atom.workspace
  maybe (pure unit) (\ed -> do
    text <- getText ed
    sendText pane proc text
    ) activeEditor

sendLine :: forall eff. PscPane -> ChildProcess -> Eff (PsciEff eff) Unit
sendLine = sendText' $ \ed -> do
  pos <- getCursorBufferPosition ed
  let p c = mkPoint (getRow pos) c
  text <- getTextInRange ed (mkRange (p 0) (p 1000))
  moveDown ed 1
  moveToBeginningOfLine ed
  pure text

closePsci :: forall eff. PscPane -> ChildProcess -> Eff (PsciEff eff) Unit
closePsci pane proc = do
  sendText pane proc ":q"
  end (stdin proc) (pure unit)

sendSelection :: forall eff. PscPane -> ChildProcess -> Eff (PsciEff eff) Unit
sendSelection = sendText' getSelectedText

startRepl :: forall eff. Ref (Maybe PscPane) -> Ref (Maybe ChildProcess) -> Aff (PsciEff eff) Unit
startRepl paneRef psciRef = do
  atom <- liftEff'' getAtom
  let notifyErr :: forall eff1. String -> Eff (console :: CONSOLE, note :: NOTIFY | eff1) Unit
      notifyErr msg = do
        log msg
        addError atom.notifications msg

  pane :: PscPane <- unsafeCoerce openPsci

  liftEff'' do
    writeRef paneRef $ Just pane
    log "Started PSCI"
    grammar <- grammarForScopeName atom.grammars "source.purescript.psci"
    pure unit

  cmd <- liftEff'' $ runExcept <$> readString <$> getConfig atom.config "ide-purescript.psciCommand"
  rootPath <- liftEff'' getProjectRoot
  let command = case cmd, regex "\\s+" noFlags of
                  Right cmd', Right r -> split r cmd'
                  _, _ -> []
  psciProcess <- liftEff'' $ case rootPath, uncons command of
    Just _, Just { head: bin, tail: args } ->
      Just <$> spawn bin args (defaultSpawnOptions { cwd = rootPath })
    Nothing, _ -> Nothing <$ notifyErr "Couldn't find project root, are you in a PureScript project?"
    _, Nothing -> Nothing <$ (notifyErr $ "Couldn't parse PSCI command: " <> show command)
  liftEff'' $ maybe (pure unit) (writeRef psciRef <<< Just) psciProcess
  liftEff'' $ maybe (pure unit) (\proc -> catchException (\e -> notifyErr $ "Couldn't launch PSCI: " <> show e) $ do
    onDataString (stdout proc) UTF8 (appendText pane)
    onDataString (stderr proc) UTF8 (appendText pane)
    onError proc (notifyErr <<< show <<< toStandardError)
    onClose proc (\exit ->
      case exit of
        Normally 0 -> log "psci exited successfully"
        _ -> notifyErr "PSCI exited abnormally"
      )
    editorElt <- querySelector (QuerySelector ".psci-input atom-text-editor") (elementToParentNode pane.element)
    case editorElt of
      Just editorElt' -> (do
        editor <- toMaybe <$> getModel editorElt'
        maybe (pure unit) (\ed -> do
          addCommand' atom.commands editorElt' "core:confirm" (\_ -> do
            text <- getText ed
            _ <- setText ed ""
            sendText pane proc text
          )
        ) editor
      )
      Nothing -> pure unit
    log "Started PSCI"
  ) psciProcess

  where liftEff'' :: forall a. Eff (PsciEff eff) a -> Aff (PsciEff eff) a
        liftEff'' = liftEff

getProjectRoot :: forall eff. Eff (project :: PROJECT, note :: NOTIFY, fs :: FS | eff) (Maybe String)
getProjectRoot = do
  atom <- getAtom
  paths <- getPaths atom.project
  dirs <- catMaybes <$> traverse getRoot paths
  pure $ _.head <$> uncons dirs

activate :: forall eff. Eff (PsciEff eff) Unit
activate = do
  atom <- getAtom
  paneRef <- newRef (Nothing :: Maybe PscPane)
  psciRef <- newRef (Nothing :: Maybe ChildProcess)

  let close = do
        pane <- liftEff $ readRef paneRef
        psci <- liftEff $ readRef psciRef
        case pane, psci of
          Just e, Just p -> do
            closePsci e p
            writeRef psciRef Nothing
            writeRef paneRef Nothing
            atomPane <- paneForItem atom.workspace e
            case atomPane of
              Just pp -> void $ destroyItem pp e
              Nothing -> pure unit
          _, _ -> pure unit


  let runCmd c = do
        pane <- liftEff $ readRef paneRef
        psci <- liftEff $ readRef psciRef
        case pane, psci of
          Just e, Just p -> liftEff $ c e p
          _, _ -> do
            startRepl paneRef psciRef
            runCmd c

  let open = launchAffAndRaise $ startRepl paneRef psciRef
  let reset = do
        ed <- readRef paneRef
        close
        maybe (pure unit) clearText ed
        open

  let cmd isEditor name action = addCommand atom.commands "atom-workspace" ("ide-purescript:psci-"<>name) (const action)
        where scope = if isEditor then "atom-text-editor" else "atom-workspace"
  cmd false "open" $ reset
  cmd true  "send-line" $ launchAffAndRaise $ runCmd sendLine
  cmd true  "send-selection" $ launchAffAndRaise $ runCmd sendSelection
  cmd true  "reset" $ reset
