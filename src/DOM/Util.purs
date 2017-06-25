module DOM.Util where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)

foreign import setTimeout :: forall eff. Int -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
