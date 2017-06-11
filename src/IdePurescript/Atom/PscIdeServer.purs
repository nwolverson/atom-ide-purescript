module IdePurescript.Atom.PscIdeServer where

import Prelude

import Atom.Atom (Atom, getAtom)
import Atom.Config (CONFIG)
import Atom.NotificationManager (NOTIFY)
import Atom.Project (PROJECT)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe)
import IdePurescript.Atom.Config (addNpmPath, effectiveServerExe, getSrcGlob, usePurs)
import IdePurescript.Atom.Util (logMsg, notify)
import IdePurescript.PscIdeServer as P
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

type ServerEff e = ( project :: PROJECT
                   , note :: NOTIFY
                   , net :: NET
                   , fs :: FS
                   , exception :: EXCEPTION
                   , random :: RANDOM
                   , config :: CONFIG
                   , process :: PROCESS
                   , cp :: CHILD_PROCESS
                   , console :: CONSOLE
                   , avar :: AVAR
                   , buffer :: BUFFER | e )

-- | Start a psc-ide server instance, or find one already running on the right path.
-- | Returns an Eff that can be evaluated to close the server later.
startServer :: forall eff eff'. String -> Aff (ServerEff eff) { quit :: P.QuitCallback eff', port :: Maybe Int }
startServer path = do
  atom <- liftEff (getAtom :: Eff (ServerEff eff) Atom)
  server <- liftEff effectiveServerExe
  glob <- liftEff getSrcGlob
  addNpmPath <- liftEff addNpmPath
  usePurs' <- liftEff usePurs
  P.startServer' path server addNpmPath usePurs' glob (notify atom.notifications) logMsg
