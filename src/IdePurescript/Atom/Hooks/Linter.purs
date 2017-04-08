module IdePurescript.Atom.Hooks.Linter where

import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn1)
import IdePurescript.Atom.Build (AtomLintMessage)
import Prelude (Unit)

foreign import data LinterRegistry :: *

-- | Referred to in linter documentation as IndieDelegate
foreign import data LinterIndie :: *

foreign import data EditorLinter :: *
foreign import data LINTER :: !

-- | Argument type of the consumeIndie entry-point function
type RegisterIndie = forall eff. EffFn1 (linter:: LINTER | eff) { name :: String } LinterIndie

foreign import clearMessages :: forall eff. LinterIndie -> Eff (linter :: LINTER | eff) Unit

-- | Set messages for a particular file, leaving others unchanged
foreign import setMessages :: forall eff. LinterIndie -> String -> Array (AtomLintMessage eff) -> Eff (linter :: LINTER | eff) Unit

-- | Set all messages, clearing any existing
foreign import setAllMessages :: forall eff. LinterIndie -> Array (AtomLintMessage eff) -> Eff (linter :: LINTER | eff) Unit

foreign import getMessages :: forall eff. EditorLinter -> Eff (linter :: LINTER | eff) (Array (AtomLintMessage eff))
