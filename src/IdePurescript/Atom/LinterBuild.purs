module IdePurescript.Atom.LinterBuild where

import Prelude
import Control.Monad.Eff

import Control.Monad.Eff.Class
import Control.Monad.Aff

import Control.Monad.Aff.Class
import Atom.Config
import Data.Either
import Data.Maybe
import Data.String.Regex
import Data.String(trim)
import Data.Foreign(readString)
import IdePurescript.Atom.Build
import Data.Array(uncons,null,length,catMaybes, filterM)
import Atom.NotificationManager
import Atom.Atom
import Node.ChildProcess ( CHILD_PROCESS)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Atom.Project
import Control.Monad
import Data.Traversable (traverse)
import Node.Path as P
import Node.FS as FS
import Node.FS.Sync as FS
import IdePurescript.Atom.Hooks.Linter


getProjectRoot :: forall eff. Eff (project :: PROJECT, note :: NOTIFY, fs :: FS.FS | eff) (Maybe String)
getProjectRoot = do
  atom <- getAtom
  paths <- getPaths atom.project
  dirs <- catMaybes <$> traverse getRoot paths
  validDirs <- filterM validDir dirs
  case uncons validDirs of
    Nothing -> do
      addWarning atom.notifications "Doesn't look like a purescript project - didn't find any src dir"
      pure Nothing
    Just { head: dir, tail } -> do
      when (not $ null tail) $ (addWarning atom.notifications $ "Multiple project roots, using first: " ++ dir)
      when (null tail && length dirs > 1) $ (addWarning atom.notifications $ "Multiple project roots but only 1 looks valid: " ++ dir)
      let output = P.concat [dir, "output"]
      outputExists <- FS.exists output
      when (not outputExists) $ (addWarning atom.notifications $  "Doesn't look like a project has been built - didn't find: " ++ output)
      pure $ Just dir
  where

  validDir :: forall eff. P.FilePath -> Eff eff Boolean
  validDir = const $ pure true
  --   validDir = (d) ->
  --     if d
  --       files = glob.sync("src/**/*.purs", {cwd: d.path})
  --       files && files.length > 0
  --     else
  --       false

  getRoot :: forall eff. P.FilePath -> Eff (fs :: FS.FS | eff) (Maybe P.FilePath)
  getRoot path = Debug.Trace.trace path \_ ->
    let parent = getParent path
        src = P.concat [path, "src"] in
    if path == "" || path == parent then
      pure Nothing
    else do
      exists <- FS.exists src
      if exists then pure $ Just path else getRoot parent

  getParent :: P.FilePath -> P.FilePath
  getParent p = P.concat [p, ".."]

type LintEff e = (cp :: CHILD_PROCESS, console :: CONSOLE, ref :: REF, config :: CONFIG, note :: NOTIFY, linter :: LINTER| e)

lint :: forall eff. Config -> String -> LinterIndie -> Eff (LintEff eff) Unit
lint config projdir linter = do
  fullBuildPath <- getConfig config "ide-purescript.buildCommand"
  let pathStr = either
  let buildCommand = either (const []) (split (regex "\\s+" noFlags) <<< trim) $ readString fullBuildPath
  atom <- getAtom
  runAff ignore ignore $ case uncons buildCommand of
    Just { head: command, tail: args } -> do
      res <- linterBuild { command, args, directory: projdir }
      liftEffA $ void $ case res of
        { result, messages } -> do
          deleteMessages linter
          setMessages linter messages
          --   @editors.onCompiled messages
          if result == "success" then addInfo atom.notifications "Building PureScript"
            else addWarning atom.notifications "PureScript build completed with errors"

    Nothing -> liftEffA $ addError atom.notifications "Error parsing PureScript build command"
  where
  ignore :: forall a e. a -> Eff e Unit
  ignore = const $ pure unit

  liftEffA :: forall a. Eff (LintEff eff) a -> Aff (LintEff eff) a
  liftEffA = liftEff
    -- catch on runAff ?
    --   atom.notifications.addError "Error running build command '#{command}'. Check configuration.\n" + err
