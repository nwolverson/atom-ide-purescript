module IdePurescript.Atom.Psci where

import Prelude
import Atom.Atom (getAtom)
import Atom.CommandRegistry (addCommand', COMMAND, addCommand)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (setText, getText, TextEditor, EDITOR, getSelectedText, moveToBeginningOfLine, moveDown, getTextInRange, getCursorBufferPosition)
import Atom.Grammar (GRAMMAR)
import Atom.GrammarRegistry (grammarForScopeName)
import Atom.NotificationManager (addError, NOTIFY)
import Atom.Point (getRow, mkPoint)
import Atom.Project (PROJECT)
import Atom.Range (mkRange)
import Atom.Workspace (getActiveTextEditor, WORKSPACE, defaultOpenOptions, open, addOpener)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (whileE, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (catchException, error)
import Control.Monad.Eff.Ref (readRef, writeRef, Ref, newRef, REF)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setClassName, setAttribute)
import DOM.Node.Node (setTextContent, firstChild, removeChild, hasChildNodes, appendChild)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToParentNode, elementToNode, Element)
import DOM.Util (setScrollTop, getScrollHeight)
import Data.Array (uncons)
import Data.Either (either)
import Data.Foreign (readString)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.Nullable (toNullable, Nullable, toMaybe)
import Data.String (indexOf, trim)
import Data.String.Regex (noFlags, regex, split)
import IdePurescript.Atom.Imports (launchAffAndRaise)
import IdePurescript.Atom.LinterBuild (getProjectRoot)
import Node.ChildProcess (Exit(Normally), onClose, onError, stdin, ChildProcess, CHILD_PROCESS, stderr, stdout, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Stream (end, writeString, onDataString)
import Unsafe.Coerce (unsafeCoerce)

foreign import init :: forall eff. Eff eff Unit

type PscPane = { getTitle :: Unit -> String, element :: Element }

createElement' :: forall eff. String -> Eff (dom :: DOM | eff) Element
createElement' elt = do
  doc <- htmlDocumentToDocument <$> (window >>= document)
  createElement elt doc

foreign import getModel :: forall eff. Element -> Eff (dom :: DOM | eff) (Nullable TextEditor)

opener :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable PscPane)
opener s =
  case indexOf "purescript://psci" s of
    Just 0 -> do
      div <- createElement' "div"
      setClassName "psci-pane" div
      topDiv <- createElement' "div"
      setClassName "psci-lines" topDiv
      appendChild (elementToNode topDiv) (elementToNode div)
      bottomDiv <- createElement' "div"
      setClassName "psci-input padded inset-panel" bottomDiv

      editorElt <- createElement' "atom-text-editor"
      setAttribute "mini" "true" editorElt
      appendChild (elementToNode editorElt) (elementToNode bottomDiv)

      appendChild (elementToNode bottomDiv) (elementToNode div)
      pure $ toNullable $ Just { getTitle: \_ -> "PSCI", element: div }
    _ -> pure $ toNullable Nothing

openPsci :: forall eff. Aff (ref :: REF, console :: CONSOLE, workspace :: WORKSPACE, note :: NOTIFY, dom :: DOM | eff) TextEditor
openPsci = makeAff \err cb -> do
  atom <- getAtom
  addOpener atom.workspace (unsafeCoerce opener)
  open atom.workspace "purescript://psci" (defaultOpenOptions { split = "right", activatePane = false }) cb (err $ error "Can't Open PSCI")

type PsciEff eff =
  ( ref :: REF
  , console :: CONSOLE
  , workspace :: WORKSPACE
  , note :: NOTIFY
  , grammar :: GRAMMAR
  , editor :: EDITOR
  , config :: CONFIG
  , fs :: FS
  , project :: PROJECT
  , cp :: CHILD_PROCESS
  , dom :: DOM
  , command :: COMMAND | eff)

appendText :: forall eff. PscPane  -> String -> Eff (PsciEff eff) Unit
appendText {element} text = do
  div <- createElement' "div"
  setClassName "psci-line" div
  setTextContent text (elementToNode div)
  lines <- toMaybe <$> querySelector ".psci-lines" (elementToParentNode element)
  maybe (pure unit) (\lines' -> do
    appendChild (elementToNode div) (elementToNode lines')
    height <- getScrollHeight lines'
    setScrollTop lines' height) lines

clearText :: forall eff. PscPane -> Eff (PsciEff eff) Unit
clearText {element} = do
  let node = elementToNode element
  whileE (hasChildNodes node) do
    child <- toMaybe <$> firstChild node
    maybe (pure unit) (void <<< flip removeChild node) child

sendText :: forall eff. PscPane -> ChildProcess -> String -> Eff (PsciEff eff) Unit
sendText pane proc text = do
  let text' = trim text ++ "\n"
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
  pane :: PscPane <- unsafeCoerce openPsci

  liftEff'' (do
    writeRef paneRef $ Just pane
    log "Started PSCI"
    grammar <- grammarForScopeName atom.grammars "source.purescript.psci"
    pure unit
    -- setGrammar pane grammar
    )

  cmd <- liftEff'' $ readString <$> getConfig atom.config "ide-purescript.psciCommand"
  rootPath <- liftEff'' getProjectRoot
  let command = either (const []) (split (regex "\\s+" noFlags) <<< trim) $ cmd
  psciProcess <- liftEff'' $ case rootPath, uncons command of
    Just _, Just { head: bin, tail: args } ->
       Just <$> spawn bin args (defaultSpawnOptions { cwd = rootPath })
    _, _ -> pure Nothing
  liftEff'' $ maybe (pure unit) (writeRef psciRef <<< Just) psciProcess
  liftEff'' $ maybe (pure unit) (\proc -> catchException (const $ pure unit) $ do
    onDataString (stdout proc) UTF8 (appendText pane)
    onDataString (stderr proc) UTF8 (appendText pane)
    onError proc (\err -> log $ "PSCI error")
    onClose proc (\exit ->
      case exit of
        Normally 0 -> log "psci exited successfully"
        _ -> addError atom.notifications "PSCI exited abnormally"
      )
    editorElt <- toMaybe <$> querySelector ".psci-input atom-text-editor" (elementToParentNode pane.element)
    case editorElt of
      Just editorElt' -> (do
        editor <- toMaybe <$> getModel editorElt'
        maybe (pure unit) (\ed -> do
          addCommand' atom.commands editorElt' "core:confirm" (\_ -> do
            text <- getText ed
            setText ed ""
            sendText pane proc text
          )
        ) editor
      )
      Nothing -> pure unit
  ) psciProcess

  where liftEff'' :: forall a. Eff (PsciEff eff) a -> Aff (PsciEff eff) a
        liftEff'' = liftEff

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

  let cmd isEditor name action = addCommand atom.commands "atom-workspace" ("psci:"++name) (const action)
        where scope = if isEditor then "atom-text-editor" else "atom-workspace"
  cmd false "open" $ open
  cmd true  "send-line" $ launchAffAndRaise $ runCmd sendLine
  cmd true  "send-selection" $ launchAffAndRaise $ runCmd sendSelection
  cmd true  "reset" $ reset
