module IdePurescript.Atom.PscIdeServer where

import Prelude
import IdePurescript.PscIdeServer as P
import Atom.Atom (getAtom)
import Atom.Config (CONFIG, getConfig)
import Atom.NotificationManager (addWarning, NOTIFY, addError, addSuccess, addInfo)
import Atom.Project (PROJECT)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (length, head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (readInt, readString)
import Data.Functor ((<$))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import IdePurescript.Atom.LinterBuild (getProjectRoot)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS, lookupEnv)
import PscIde (NET)
import PscIde.Server (findBins, Executable(Executable))

type ServerEff e = ( project :: PROJECT
                   , note :: NOTIFY
                   , net :: NET
                   , fs :: FS
                   , config :: CONFIG
                   , process :: PROCESS
                   , cp :: CHILD_PROCESS
                   , console :: CONSOLE
                   , avar ::AVAR
                   , buffer :: BUFFER | e )

-- | Start a psc-ide server instance, or find one already running on the right path.
-- | Returns an Eff that can be evaluated to close the server later.
startServer :: forall eff eff'. Aff (ServerEff eff)
  (Eff (err :: EXCEPTION, net :: NET, cp :: CHILD_PROCESS | eff') Unit)
startServer = do
  atom <- liftEffS getAtom
  path <- liftEffS getProjectRoot
  portRaw <- liftEffS $ readInt <$> getConfig atom.config "ide-purescript.pscIdePort"
  serverRaw <- liftEffS $ readString <$> getConfig atom.config "ide-purescript.pscIdeServerExe"
  case { portRaw, serverRaw, path } of
    { portRaw: Right port, serverRaw: Right exe, path: Just path' } -> do
        serverBins <- findBins exe
        case head serverBins of
          Nothing -> do
            processPath <- liftEffS $ lookupEnv "PATH"
            liftEffS $ addError atom.notifications $ "Couldn't find psc-ide-server, check PATH. Looked for: "
              <> exe <> " in PATH: " <> fromMaybe "" processPath
            pure $ pure unit
          Just (Executable bin _) -> do
            liftEff $ log $ "Resolved psc-ide-server:"
            traverse_ (\(Executable x vv) -> do
              liftEff $ log $ x <> ": " <> fromMaybe "ERROR" vv) serverBins
            liftEff $ when (length serverBins > 1) $ addWarning atom.notifications $ "Found multiple psc-ide-server executables; using " <> bin
            res <- P.startServer bin port path'
            childProc <- liftEff $ case res of
              P.CorrectPath -> Nothing <$ addInfo atom.notifications "Found existing psc-ide-server with correct path"
              P.WrongPath wrongPath -> Nothing <$ (addError atom.notifications $ "Found existing psc-ide-server with wrong path: '" <>wrongPath<>"'. Correct, kill or configure a different port, and restart.")
              P.Started cp -> Just cp <$ addSuccess atom.notifications "Started psc-ide-server"
              P.Closed -> Nothing <$ addInfo atom.notifications "psc-ide-server exited with success code"
              P.StartError err -> Nothing <$ (addError atom.notifications $ "Could not start psc-ide-server process. Check the configured port number is valid.\n" <>err)
            case childProc of
              Nothing -> pure $ pure unit
              Just cp -> pure $ void $ launchAff $ P.stopServer port cp -- $ kill SIGKILL cp
    _ -> pure $ pure unit

  where liftEffS :: forall a. Eff (ServerEff eff) a -> Aff (ServerEff eff) a
        liftEffS = liftEff
