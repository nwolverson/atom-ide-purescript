module IdePurescript.Atom.PscIdeServer where

import Prelude
import IdePurescript.PscIdeServer as P
import Atom.Atom (Atom, getAtom)
import Atom.Config (CONFIG, getConfig)
import Atom.NotificationManager (NotificationManager, addWarning, NOTIFY, addError, addSuccess, addInfo)
import Atom.Project (PROJECT)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, info, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe)
import Data.Bifunctor (rmap)
import Data.Either (either)
import Data.Foreign (readArray, readString, readBoolean)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

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

notify :: forall eff. NotificationManager -> P.Notify (note :: NOTIFY | eff)
notify = flip case _ of
  P.Success -> addSuccess
  P.Info -> addInfo
  P.Warning -> addWarning
  P.Error -> addError

logMsg :: forall eff. P.Notify (console :: CONSOLE | eff)
logMsg = case _ of
  P.Success -> log
  P.Info -> info
  P.Warning -> warn
  P.Error -> error

-- | Start a psc-ide server instance, or find one already running on the right path.
-- | Returns an Eff that can be evaluated to close the server later.
startServer :: forall eff eff'. String -> Aff (ServerEff eff) { quit :: P.QuitCallback eff', port :: Maybe Int }
startServer path = do
  atom <- liftEff (getAtom :: Eff (ServerEff eff) Atom)
  serverRaw <- liftEff $ readString <$> getConfig atom.config "ide-purescript.pscIdeServerExe"
  pursRaw <- liftEff $ readString <$> getConfig atom.config "ide-purescript.pursExe"
  srcGlob <- liftEff $ readArray <$> getConfig atom.config "ide-purescript.pscSourceGlob"
  usePurs <- liftEff $ readBoolean <$> getConfig atom.config "ide-purescript.useCombinedExe"
  addNpmPath <- liftEff $ readBoolean <$> getConfig atom.config "ide-purescript.addNpmPath"
  let srcGlob' = rmap (mapMaybe $ (either (const Nothing) Just) <<< runExcept <<< readString) $ runExcept srcGlob
  let glob = either (const ["src/**/*.purs", "bower_components/**/*.purs"]) id srcGlob'
  let usePurs' = either (const false) id $ runExcept usePurs
  let addNpmPath' = either (const false) id $ runExcept addNpmPath

  let server = if usePurs' then
                  either (const "purs") id $ runExcept pursRaw
                else
                  either (const "psc-ide-server") id $ runExcept serverRaw

  P.startServer' path server addNpmPath' usePurs' glob (notify atom.notifications) logMsg
