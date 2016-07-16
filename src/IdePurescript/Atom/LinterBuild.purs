module IdePurescript.Atom.LinterBuild where

import Prelude
import Node.Path as P
import Atom.Atom (getAtom)
import Atom.Config (Config, CONFIG, getConfig)
import Atom.NotificationManager (NOTIFY, addError)
import Atom.Project (PROJECT, getPaths)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Error.Class (catchError)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (uncons, catMaybes)
import Data.Either (Either(Right), either)
import Data.Foreign (readBoolean, readString)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (trim)
import Data.String.Regex (noFlags, regex, split)
import Data.Traversable (traverse)
import IdePurescript.Atom.Build (AtomLintMessage, linterBuild, toLintResult)
import IdePurescript.Atom.BuildStatus (BuildStatus(Failure, Errors, Success, Building), updateBuildStatus)
import IdePurescript.Atom.Hooks.Linter (LinterIndie, LINTER, setMessages, deleteMessages)
import IdePurescript.Build (BuildResult, rebuild)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS) as FS
import Node.FS.Sync (exists) as FS
import PscIde (NET)

getRoot :: forall eff'. P.FilePath -> Eff (fs :: FS.FS | eff') (Maybe P.FilePath)
getRoot path =
  let parent = getParent path
      src = P.concat [path, "src"] -- Not sure this is necessary
      bower = P.concat [path, "bower.json"] in
  if path == "" || path == parent then
    pure Nothing
  else do
    hasSrc <- FS.exists src
    hasBower <- FS.exists bower
    if hasSrc || hasBower then pure $ Just path else getRoot parent

  where
    getParent :: P.FilePath -> P.FilePath
    getParent p = P.concat [p, ".."]

-- TODO: Now only used by PSCI
getProjectRoot :: forall eff. Boolean -> Eff (project :: PROJECT, note :: NOTIFY, fs :: FS.FS | eff) (Maybe String)
getProjectRoot _ = do
  atom <- getAtom
  paths <- getPaths atom.project
  dirs <- catMaybes <$> traverse getRoot paths
  pure $ _.head <$> uncons dirs

type LintEff e = (cp :: CHILD_PROCESS, console :: CONSOLE, ref :: REF, config :: CONFIG, note :: NOTIFY, linter :: LINTER, dom :: DOM, net :: NET | e)

lint :: forall eff. Maybe Int -> Maybe String -> Config -> String -> LinterIndie -> Element -> Aff (LintEff eff) (Maybe (Array AtomLintMessage))
lint port file config projdir linter statusElt = do
  atom <- liftEffA $ getAtom
  fullBuildPath <- liftEffA $ getConfig config "ide-purescript.buildCommand"
  let pathStr = either
  let buildCommand = case regex "\\s+" noFlags, readString fullBuildPath of
                      Right r, Right s -> split r $ trim $ s
                      _, _ -> []

  isFastBuild <- liftEffA $ readBoolean <$> getConfig config "ide-purescript.fastRebuild"
  let fastBuild :: Maybe (Aff (LintEff eff) BuildResult)
      fastBuild = case isFastBuild, file, port of
        Right true, Just fileName, Just p -> Just $ rebuild p fileName
        _, _, _ -> Nothing
  pure unit

  case fastBuild, uncons buildCommand of
    Just cmd, _ -> doBuild cmd
    _, Just { head: command, tail: args } -> doBuild $ linterBuild { command, args, directory: projdir }
    _, Nothing -> (do
      liftEffA $ failure "Error parsing PureScript build command"
      pure Nothing)
  where

    doBuild :: (Aff (LintEff eff) BuildResult) -> Aff (LintEff eff) (Maybe (Array AtomLintMessage))
    doBuild buildCmd =
      do
        liftEff $ status Building Nothing
        res <- toLintResult <$> buildCmd
        liftEffA $ Just <$> case res of
          { result, messages } -> do
            deleteMessages linter
            setMessages linter messages
            status (if result == "success" then Success else Errors) Nothing
            pure messages
      `catchError` \(e :: Error) -> do
        liftEffA $ failure $ "Error running PureScript build command: " <> show e
        pure Nothing

    failure :: String -> Eff (LintEff eff) Unit
    failure s = do
      atom <- getAtom
      status Failure $ Just s
      addError atom.notifications s

    status :: BuildStatus -> (Maybe String) -> Eff (LintEff eff) Unit
    status s msg = do
      updateBuildStatus statusElt s
      (if s == Failure then error else log) $ "PureScript build status: " <> show s <> (maybe "" (": " <> _)  msg)

    liftEffA :: forall a. Eff (LintEff eff) a -> Aff (LintEff eff) a
    liftEffA = liftEff
    -- catch on runAff ?
    --   atom.notifications.addError "Error running build command '#{command}'. Check configuration.\n" + err
