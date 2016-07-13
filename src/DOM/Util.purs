module DOM.Util where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Element)

foreign import getScrollTop :: forall eff. Element -> Eff (dom :: DOM | eff) Number

foreign import setScrollTop :: forall eff. Element -> Number -> Eff (dom :: DOM | eff) Unit

foreign import getScrollHeight :: forall eff. Element -> Eff (dom :: DOM | eff) Number

foreign import setTimeout :: forall eff. Int -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
