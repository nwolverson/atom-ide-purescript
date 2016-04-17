module IdePurescript.Atom.Psci where

import Prelude
import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND, addCommand)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (setText, getSelectedText, moveDown, getCursorBufferPosition, getTextInRange, moveToBeginningOfLine, TextEditor, EDITOR, setGrammar, insertText, moveToBottom)
import Atom.Grammar (GRAMMAR)
import Atom.GrammarRegistry (grammarForScopeName)
import Atom.NotificationManager (addError, NOTIFY)
import Atom.Point (getRow, mkPoint)
import Atom.Project (PROJECT)
import Atom.Range (mkRange)
import Atom.Workspace (getActiveTextEditor, WORKSPACE, defaultOpenOptions, open)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (catchException, error)
import Control.Monad.Eff.Ref (readRef, writeRef, Ref, newRef, REF)
import Data.Array (uncons)
import Data.Either (either)
import Data.Foreign (readString)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.String (trim)
import Data.String.Regex (noFlags, regex, split)
import IdePurescript.Atom.Imports (launchAffAndRaise)
import IdePurescript.Atom.LinterBuild (getProjectRoot)
import Node.ChildProcess (Exit(Normally), onClose, onError, stdin, ChildProcess, CHILD_PROCESS, stderr, stdout, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Stream (end, writeString, onDataString)

foreign import init :: forall eff. Eff eff Unit

openPsci :: forall eff. Aff (ref :: REF, console :: CONSOLE, workspace :: WORKSPACE, note :: NOTIFY | eff) TextEditor
openPsci = makeAff \err cb -> do
  atom <- getAtom
  open atom.workspace "PSCI" (defaultOpenOptions { split = "right", activatePane = false }) cb (err $ error "Can't Open PSCI")

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
  , cp :: CHILD_PROCESS | eff)

appendText :: forall eff. TextEditor -> String -> Eff (PsciEff eff) Unit
appendText editor text = do
  moveToBottom editor
  insertText editor text

sendText :: forall eff. TextEditor -> ChildProcess -> String -> Eff (PsciEff eff) Unit
sendText editor proc text = do
  let text' = trim text ++ "\n"
  insertText editor text'
  void $ writeString (stdin proc) UTF8 text' (pure unit)

sendText' :: forall eff. (TextEditor -> Eff (PsciEff eff) String) -> TextEditor -> ChildProcess -> Eff (PsciEff eff) Unit
sendText' getText editor proc  = do
  atom <- getAtom
  activeEditor <- getActiveTextEditor atom.workspace
  maybe (pure unit) (\ed -> do
    text <- getText ed
    sendText editor proc text
    ) activeEditor

sendLine :: forall eff. TextEditor -> ChildProcess -> Eff (PsciEff eff) Unit
sendLine = sendText' $ \ed -> do
  pos <- getCursorBufferPosition ed
  let p c = mkPoint (getRow pos) c
  text <- getTextInRange ed (mkRange (p 0) (p 1000))
  moveDown ed 1
  moveToBeginningOfLine ed
  pure text

closePsci :: forall eff. TextEditor -> ChildProcess -> Eff (PsciEff eff) Unit
closePsci editor proc = do
  sendText editor proc ":q"
  end (stdin proc) (pure unit)

sendSelection :: forall eff. TextEditor -> ChildProcess -> Eff (PsciEff eff) Unit
sendSelection = sendText' getSelectedText

startRepl :: forall eff. Ref (Maybe TextEditor) -> Ref (Maybe ChildProcess) -> Aff (PsciEff eff) Unit
startRepl editorRef psciRef = do
  atom <- liftEff'' getAtom
  editor <- openPsci
  liftEff'' (do
    writeRef editorRef $ Just editor
    log "Started PSCI"
    grammar <- grammarForScopeName atom.grammars "source.purescript.psci"
    setGrammar editor grammar)

  cmd <- liftEff'' $ readString <$> getConfig atom.config "ide-purescript.psciCommand"
  rootPath <- liftEff'' getProjectRoot
  let command = either (const []) (split (regex "\\s+" noFlags) <<< trim) $ cmd
  psciProcess <- liftEff'' $ case rootPath, uncons command of
    Just _, Just { head: bin, tail: args } ->
       Just <$> spawn bin args (defaultSpawnOptions { cwd = rootPath })
    _, _ -> pure Nothing
  liftEff'' $ maybe (pure unit) (writeRef psciRef <<< Just) psciProcess
  liftEff'' $ maybe (pure unit) (\proc -> catchException (const $ pure unit) $ do
    onDataString (stdout proc) UTF8 (appendText editor)
    onDataString (stderr proc) UTF8 (appendText editor)
    onError proc (\err -> log $ "PSCI error")
    onClose proc (\exit ->
      case exit of
        Normally 0 -> log "psci exited successfully"
        _ -> addError atom.notifications "PSCI exited abnormally"
      )
    ) psciProcess

  where liftEff'' :: forall a. Eff (PsciEff eff) a -> Aff (PsciEff eff) a
        liftEff'' = liftEff

activate :: forall eff. Eff (PsciEff (command :: COMMAND | eff)) Unit
activate = do
  atom <- getAtom
  editorRef <- newRef (Nothing :: Maybe TextEditor)
  psciRef <- newRef (Nothing :: Maybe ChildProcess)

  let close = do
        editor <- liftEff $ readRef editorRef
        psci <- liftEff $ readRef psciRef
        case editor, psci of
          Just e, Just p -> do
            closePsci e p
            writeRef psciRef Nothing
            writeRef editorRef Nothing
          _, _ -> pure unit

  let runCmd c = do
        editor <- liftEff $ readRef editorRef
        psci <- liftEff $ readRef psciRef
        case editor, psci of
          Just e, Just p -> liftEff $ c e p
          _, _ -> do
            startRepl editorRef psciRef
            runCmd c

  let open = launchAffAndRaise $ startRepl editorRef psciRef
  let reset = do
        ed <- readRef editorRef
        close
        case ed of
          Just ed' -> void $ setText ed' ""
          _ -> pure unit
        open

  let cmd isEditor name action = addCommand atom.commands "atom-workspace" ("psci:"++name) (const action)
        where scope = if isEditor then "atom-text-editor" else "atom-workspace"
  cmd false "open" $ open
  cmd true  "send-line" $ launchAffAndRaise $ runCmd sendLine
  cmd true  "send-selection" $ launchAffAndRaise $ runCmd sendSelection
  cmd true  "reset" $ reset
