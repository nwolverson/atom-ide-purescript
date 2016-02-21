module IdePurescript.Atom.QuickFixes (showQuickFixes) where

import Prelude (Unit)
import Data.Function.Eff (EffFn3, runEffFn3)
import Control.Monad.Eff (Eff)
import Atom.Editor (TextEditor)
import IdePurescript.Atom.Hooks.Linter (LinterInternal)
import IdePurescript.Atom.Build (AtomLintMessage)

foreign import showQuickFixesImpl :: forall eff. EffFn3 eff TextEditor LinterInternal (Array AtomLintMessage) Unit

-- EFF type
showQuickFixes :: forall eff. TextEditor -> LinterInternal -> Array AtomLintMessage -> Eff eff Unit
showQuickFixes = runEffFn3 showQuickFixesImpl
