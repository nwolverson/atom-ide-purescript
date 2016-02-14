module IdePurescript.Atom.Hooks.Linter where

import Prelude (Unit)
import Control.Monad.Eff(Eff)
import IdePurescript.Atom.Build (AtomLintMessage)

foreign import data LinterRegistry :: *
foreign import data LinterIndie :: *
foreign import data LinterInternal :: *
foreign import data LINTER :: !

foreign import register :: forall eff. LinterRegistry -> { name :: String } -> Eff (linter:: LINTER | eff) LinterIndie

foreign import deleteMessages :: forall eff. LinterIndie -> Eff (linter :: LINTER | eff) Unit

foreign import setMessages :: forall eff. LinterIndie -> Array AtomLintMessage -> Eff (linter :: LINTER | eff) Unit
