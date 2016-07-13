module IdePurescript.Atom.PscIdeServer where

import Prelude
import IdePurescript.PscIdeServer as P
import Atom.Atom (getAtom)
import Atom.Config (CONFIG, getConfig)
import Atom.NotificationManager (addWarning, NOTIFY, addError, addSuccess, addInfo)
import Atom.Project (PROJECT)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (length, head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (readString)
import Data.Functor ((<$))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
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
                   , err :: EXCEPTION
                   , random :: RANDOM
                   , config :: CONFIG
                   , process :: PROCESS
                   , cp :: CHILD_PROCESS
                   , console :: CONSOLE
                   , avar :: AVAR
                   , buffer :: BUFFER | e )

type QuitCallback eff = (Eff (err :: EXCEPTION, net :: NET, cp :: CHILD_PROCESS, fs :: FS | eff) Unit)

-- | Start a psc-ide server instance, or find one already running on the right path.
-- | Returns an Eff that can be evaluated to close the server later.
startServer :: forall eff eff'. String -> Aff (ServerEff eff) { quit :: QuitCallback eff', port :: Maybe Int }
startServer path = do
  atom <- liftEffS getAtom
  serverRaw <- liftEffS $ readString <$> getConfig atom.config "ide-purescript.pscIdeServerExe"
  case serverRaw of
    Right exe -> do
        serverBins <- findBins exe
        case head serverBins of
          Nothing -> do
            processPath <- liftEffS $ lookupEnv "PATH"
            liftEffS $ addError atom.notifications $ "Couldn't find psc-ide-server, check PATH. Looked for: "
              <> exe <> " in PATH: " <> fromMaybe "" processPath
            pure { quit: pure unit, port: Nothing }
          Just (Executable bin _) -> do
            liftEff $ log $ "Resolved psc-ide-server:"
            traverse_ (\(Executable x vv) -> do
              liftEff $ log $ x <> ": " <> fromMaybe "ERROR" vv) serverBins
            liftEff $ when (length serverBins > 1) $ addWarning atom.notifications $ "Found multiple psc-ide-server executables; using " <> bin
            res <- P.startServer bin path
            let noRes = { quit: pure unit, port: Nothing }
            liftEff $ case res of
              P.CorrectPath usedPort -> { quit: pure unit, port: Just usedPort } <$ addInfo atom.notifications ("Found existing psc-ide-server with correct path on port " <> show usedPort)
              P.WrongPath usedPort wrongPath -> do
                addError atom.notifications $ "Found existing psc-ide-server on port '" <> show usedPort <> "' with wrong path: '" <> wrongPath
                  <> "'. Correct, kill or configure a different port, and restart."
                pure noRes
              P.Started usedPort cp -> do
                addSuccess atom.notifications $ "Started psc-ide-server (port " <> show usedPort <> ")"
                pure
                  { quit: void $ runAff (\_ -> pure unit) (\_ -> pure unit) $ P.stopServer usedPort path cp
                  , port: Just usedPort
                  }
              P.Closed -> noRes <$ addInfo atom.notifications "psc-ide-server exited with success code"
              P.StartError err -> noRes <$ (addError atom.notifications $ "Could not start psc-ide-server process. Check the configured port number is valid.\n" <> err)
    _ -> do
      liftEffS $ log "Failed to get config to start server"
      pure { quit: pure unit, port: Nothing }

  where liftEffS :: forall a. Eff (ServerEff eff) a -> Aff (ServerEff eff) a
        liftEffS = liftEff
