module Node.Which (which) where

import Prelude
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Eff (mkEffFn1, runEffFn3, EffFn1, EffFn3)
import Node.FS (FS)


foreign import whichImpl :: forall eff. EffFn3 (fs :: FS | eff)
  String
  (EffFn1 (fs :: FS | eff) (Array String) Unit)
  (EffFn1 (fs :: FS | eff) Error Unit)
  Unit

which :: forall eff. String -> Aff (fs :: FS | eff) (Array String)
which s = makeAff $ \err succ ->
 runEffFn3 whichImpl s (mkEffFn1 succ) (mkEffFn1 err)
