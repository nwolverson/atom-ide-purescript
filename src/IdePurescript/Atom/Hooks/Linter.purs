module IdePurescript.Atom.Hooks.Linter where

import Atom.Editor (TextEditor)
import Atom.Range (Range)
import Control.Monad.Eff (Eff)
import Data.Nullable (Nullable)
import IdePurescript.Atom.Build (AtomLintMessage)
import Prelude (Unit)

foreign import data LinterRegistry :: *
foreign import data LinterIndie :: *
foreign import data LinterInternal :: *

foreign import data EditorLinter :: *
foreign import data LINTER :: !

foreign import register :: forall eff. LinterRegistry -> { name :: String } -> Eff (linter:: LINTER | eff) LinterIndie

foreign import deleteMessages :: forall eff. LinterIndie -> Eff (linter :: LINTER | eff) Unit

foreign import setMessages :: forall eff. LinterIndie -> Array AtomLintMessage -> Eff (linter :: LINTER | eff) Unit

foreign import getMessages :: forall eff. EditorLinter -> Eff (linter :: LINTER | eff) (Array AtomLintMessage)

foreign import getEditorLinter :: forall eff. LinterInternal -> TextEditor -> Eff (linter :: LINTER | eff) EditorLinter

foreign import getMarkerBufferRange :: forall eff. EditorLinter -> AtomLintMessage -> Eff (linter :: LINTER | eff) (Nullable Range)
