module IdePurescript.Atom.Imports where

import Prelude

import Atom.Atom (getAtom)
import Atom.CommandRegistry (COMMAND)
import Atom.Editor (TextEditor, EDITOR, setText, getPath, getText, getBuffer)
import Atom.NotificationManager (NOTIFY)
import Atom.Workspace (WORKSPACE, getActiveTextEditor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (Ref, REF, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foreign (readArray, readString, toForeign)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Nullable (toMaybe, toNullable)
import Data.Traversable (traverse)
import IdePurescript.Atom.Editor (getActivePosInfo, getLinePosition, getToken)
import IdePurescript.Atom.Hooks.LanguageClient (LanguageClientConnection, executeCommand)
import IdePurescript.Atom.PromptPanel (addPromptPanel)
import IdePurescript.Atom.SelectView (selectListViewStatic)
import IdePurescript.Atom.Util (launchAffAndRaise)
import LanguageServer.IdePurescript.Commands (addCompletionImport)
import LanguageServer.Types (Command(..), DocumentUri(..))
import LanguageServer.Uri (filenameToUri)
import Node.FS (FS)
import PscIde (NET)
import Text.Smolder.HTML (li)
import Text.Smolder.Markup (text)
import Text.Smolder.Renderer.String (render)

type AddModuleEff eff = ImportEff (dom :: DOM, exception :: EXCEPTION | eff)
type ImportEff eff = (workspace :: WORKSPACE, ref :: REF,  note :: NOTIFY, net :: NET, editor :: EDITOR, fs :: FS, console :: CONSOLE, command :: COMMAND | eff)

addExplicitImport :: forall eff. LanguageClientConnection -> Eff (AddModuleEff eff) Unit
addExplicitImport conn = launchAffAndRaise $
  liftEff getActivePosInfo >>= maybe (pure unit) \{ line, pos, ed } -> do
    word <- liftEff $ getToken ed >>= maybe (pure "") (pure <<< _.word)
    ident <- addPromptPanel "Identifier" word
    path <- liftEff $ getPath ed
    case path, ident of
      Just path', Just ident' -> liftEff (filenameToUri path') >>= \path'' ->
        addIdentImport ident' path'' Nothing
      _, _ -> pure unit
  where
    addIdentImport ident uri mod = do
      let Command { command, arguments } = addCompletionImport ident mod Nothing uri
      res <- executeCommand conn { command, arguments: fromMaybe [] $ toMaybe arguments }
      case runExcept $ readArray res of
        Right forArr
          | Right arr <- runExcept $ traverse readString forArr
          -> liftEff $ selectListViewStatic view addImp (Just "module'") ({ identifier: ident, module': _ } <$> arr)
        _ -> pure unit
      where
      addImp { identifier, "module'": m } = launchAffAndRaise $ addIdentImport ident uri (Just m)
      view { identifier, "module'": m} = render $
        li $ text $ m <> "." <> identifier

addModuleImport' :: forall eff.
  LanguageClientConnection
  -> TextEditor
     -> String
        -> Eff
             ( editor :: EDITOR
             , console :: CONSOLE
             , exception :: EXCEPTION
             , note :: NOTIFY
             | eff
             )
             Unit
addModuleImport' conn editor mod =
  getPath editor >>= maybe (pure unit) \path -> do
    uri <- filenameToUri path
    launchAffAndRaise $ executeCommand conn { command: "purescript.addModuleImport",
      arguments: [ toForeign mod, toForeign $ toNullable Nothing, toForeign uri ]}

addModuleImport :: forall eff. LanguageClientConnection -> Eff (AddModuleEff eff) Unit
addModuleImport conn = launchAffAndRaise do
  modules <- executeCommand conn { command: "purescript.getAvailableModules", arguments: [] }
  atom <- liftEff getAtom
  ed <- liftEff $ getActiveTextEditor atom.workspace
  case ed, runExcept $ readArray modules of
    Just editor, Right forArr
      | Right arr <- runExcept $ traverse readString forArr
      -> liftEff $ selectListViewStatic view (addModuleImport' conn editor) Nothing arr
    _, _ -> pure unit
  where
    view x = render $
      li $ text x
