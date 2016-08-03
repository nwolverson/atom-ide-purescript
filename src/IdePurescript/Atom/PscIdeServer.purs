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
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (mapMaybe)
import Data.Bifunctor (rmap)
import Data.Either (either)
import Data.Foreign (readArray, readString)
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
notify notifications level = case level of
  P.Success -> addSuccess notifications
  P.Info -> addInfo notifications
  P.Warning -> addWarning notifications
  P.Error -> addError notifications

-- | Start a psc-ide server instance, or find one already running on the right path.
-- | Returns an Eff that can be evaluated to close the server later.
startServer :: forall eff eff'. String -> Aff (ServerEff eff) { quit :: P.QuitCallback eff', port :: Maybe Int }
startServer path = do
  atom <- liftEff (getAtom :: Eff (ServerEff eff) Atom)
  serverRaw <- liftEff $ readString <$> getConfig atom.config "ide-purescript.pscIdeServerExe"
  srcGlob <- liftEff $ readArray <$> getConfig atom.config "ide-purescript.pscSourceGlob"
  let srcGlob' = rmap (mapMaybe $ (either (const Nothing) Just) <<< readString) srcGlob
  let glob = either (const ["src/**/*.purs", "bower_components/**/*.purs"]) id srcGlob'
  let server = either (const "psc-ide-server") id serverRaw

  P.startServer' path server glob (notify atom.notifications)
