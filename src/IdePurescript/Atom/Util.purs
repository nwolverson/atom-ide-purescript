module IdePurescript.Atom.Util where

import Prelude
import IdePurescript.PscIdeServer as P
import Atom.Atom (getAtom)
import Atom.NotificationManager (NotificationManager, addWarning, NOTIFY, addError, addSuccess, addInfo)
import Control.Monad.Eff.Console (CONSOLE, error, errorShow, info, log, warn)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

launchAffAndRaise :: forall a e. Aff (note :: NOTIFY, console :: CONSOLE | e) a -> Eff (note :: NOTIFY, console :: CONSOLE | e) Unit
launchAffAndRaise = void <<< (runAff raiseError (const $ pure unit))
  where
  raiseError :: forall eff. Error -> Eff (note :: NOTIFY, console :: CONSOLE | eff) Unit
  raiseError e = do
    atom <- getAtom
    errorShow e
    addError atom.notifications (show e)

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
