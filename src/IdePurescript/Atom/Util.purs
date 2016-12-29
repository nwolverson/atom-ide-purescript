module IdePurescript.Atom.Util where

import Prelude
import Atom.Atom (getAtom)
import Atom.NotificationManager (NOTIFY, addError)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (Error)

launchAffAndRaise :: forall a e. Aff (note :: NOTIFY, console :: CONSOLE | e) a -> Eff (note :: NOTIFY, console :: CONSOLE | e) Unit
launchAffAndRaise = void <<< (runAff raiseError (const $ pure unit))
  where
  raiseError :: forall eff. Error -> Eff (note :: NOTIFY, console :: CONSOLE | eff) Unit
  raiseError e = do
    atom <- getAtom
    errorShow e
    addError atom.notifications (show e)
