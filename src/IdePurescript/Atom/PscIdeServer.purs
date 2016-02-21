module IdePurescript.Atom.PscIdeServer where

import Prelude (Unit, unit, pure, (<$>), ($), bind, (++), void)
import Data.Functor ((<$))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Maybe(Maybe(..))
import Data.Either(Either(..))
import Data.Foreign (readString, readInt)
import Node.ChildProcess (CHILD_PROCESS, kill)
import Data.Posix.Signal (Signal(SIGKILL))
import Node.FS (FS)
import Atom.Atom (getAtom)
import Atom.Config (CONFIG, getConfig)
import Atom.Project (PROJECT)
import Atom.NotificationManager (NOTIFY, addError, addSuccess, addInfo)
import IdePurescript.PscIdeServer as P
import IdePurescript.Atom.LinterBuild (getProjectRoot)
import PscIde (NET)

type ServerEff e = ( project :: PROJECT
                               , note :: NOTIFY
                               , net :: NET
                               , fs :: FS
                               , config :: CONFIG
                               , cp :: CHILD_PROCESS
                               , console :: CONSOLE
                               , avar ::AVAR | e )

startServer :: forall eff eff'. Aff (ServerEff eff) (Eff (cp :: CHILD_PROCESS | eff') Unit)
startServer = do
  atom <- liftEffS getAtom
  path <- liftEffS getProjectRoot
  portRaw <- liftEffS $ readInt <$> getConfig atom.config "ide-purescript.pscIdePort"
  serverRaw <- liftEffS $ readString <$> getConfig atom.config "ide-purescript.pscIdeServerExe"
  case { portRaw, serverRaw, path } of
    { portRaw: Right l, serverRaw: Right s, path: Just p } -> do
      --runAff (\_ -> log "error starting psc-ide-server") (\_ -> pure unit) $ do
        res <- P.startServer s l p
        childProc <- liftEff $ case res of
          P.CorrectPath -> Nothing <$ addInfo atom.notifications "Found existing psc-ide-server with correct path"
          P.WrongPath wrongPath -> Nothing <$ (addError atom.notifications $ "Found existing psc-ide-server with wrong path: '" ++wrongPath++"'. Correct, kill or configure a different port, and restart.")
          P.Started cp -> Just cp <$ addSuccess atom.notifications "Started psc-ide-server"
          P.Closed -> Nothing <$ addInfo atom.notifications "psc-ide-server exited with success code"
          P.StartError err -> Nothing <$ (addError atom.notifications $ "Could not start psc-ide-server process. Check the configured port number is valid.\n" ++err)
        case childProc of
          Nothing -> pure $ pure unit
          Just cp -> pure $ void $ kill SIGKILL cp
    _ -> pure $ pure unit

  where liftEffS :: forall a. Eff (ServerEff eff) a -> Aff (ServerEff eff) a
        liftEffS = liftEff
