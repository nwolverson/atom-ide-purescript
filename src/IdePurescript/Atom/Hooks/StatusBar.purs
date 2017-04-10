module IdePurescript.Atom.Hooks.StatusBar (StatusBar, STATUS, addLeftTile, addRightTile) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import DOM.Node.Types (Element)

foreign import data StatusBar :: Type
foreign import data STATUS :: Effect

foreign import addLeftTileImpl :: forall eff. EffFn2 (status :: STATUS | eff) StatusBar { item :: Element, priority :: Int } Unit

addLeftTile :: forall eff. StatusBar -> { item :: Element, priority :: Int } -> Eff (status :: STATUS | eff) Unit
addLeftTile = runEffFn2 addLeftTileImpl

foreign import addRightTileImpl :: forall eff. EffFn2 (status :: STATUS | eff) StatusBar { item :: Element, priority :: Int } Unit

addRightTile :: forall eff. StatusBar -> { item :: Element, priority :: Int } -> Eff (status :: STATUS | eff) Unit
addRightTile = runEffFn2 addRightTileImpl
