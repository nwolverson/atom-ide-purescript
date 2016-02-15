module IdePurescript.Atom.LinterBuild where

import Prelude (pure, ($), bind, (==), (<$>), (<<<), const, (++), not, (>), (&&), (||))

import Control.Monad (when)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Data.Array(uncons,null,length,catMaybes, filterM)
import Data.Either (either)
import Data.Foreign(readString)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String.Regex (noFlags, regex, split)
import Data.String(trim)
import Data.Traversable (traverse)

import Node.ChildProcess ( CHILD_PROCESS)
import Node.FS (FS) as FS
import Node.FS.Sync (exists) as FS
import Node.Path as P

import Atom.Atom (getAtom)
import Atom.Config (Config, CONFIG, getConfig)
import Atom.NotificationManager (NOTIFY, addError, addWarning, addSuccess, addInfo)
import Atom.Project (PROJECT, getPaths)

import IdePurescript.Atom.Build (AtomLintMessage, linterBuild)
import IdePurescript.Atom.Hooks.Linter (LinterIndie, LINTER, setMessages, deleteMessages)

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

  validDir :: forall eff'. P.FilePath -> Eff eff' Boolean
  validDir = const $ pure true
  --   validDir = (d) ->
  --     if d
  --       files = glob.sync("src/**/*.purs", {cwd: d.path})
  --       files && files.length > 0
  --     else
  --       false

  getRoot :: forall eff'. P.FilePath -> Eff (fs :: FS.FS | eff') (Maybe P.FilePath)
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

lint :: forall eff. Config -> String -> LinterIndie -> Aff (LintEff eff) (Maybe (Array AtomLintMessage))
lint config projdir linter = do
  atom <- liftEffA $ getAtom
  fullBuildPath <- liftEffA $ getConfig config "ide-purescript.buildCommand"
  let pathStr = either
  let buildCommand = either (const []) (split (regex "\\s+" noFlags) <<< trim) $ readString fullBuildPath
  case uncons buildCommand of
    Just { head: command, tail: args } -> do
      liftEff $ addInfo atom.notifications "Building PureScript"
      res <- linterBuild { command, args, directory: projdir }
      liftEffA $ Just <$> case res of
        { result, messages } -> do
          deleteMessages linter
          setMessages linter messages
          if result == "success" then addSuccess atom.notifications "PureScript build succeeded"
            else addWarning atom.notifications "PureScript build completed with errors"
          pure messages
    Nothing -> do
      liftEffA $ addError atom.notifications "Error parsing PureScript build command"
      pure Nothing
  where

  liftEffA :: forall a. Eff (LintEff eff) a -> Aff (LintEff eff) a
  liftEffA = liftEff
    -- catch on runAff ?
    --   atom.notifications.addError "Error running build command '#{command}'. Check configuration.\n" + err
