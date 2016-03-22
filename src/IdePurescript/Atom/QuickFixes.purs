module IdePurescript.Atom.QuickFixes (showQuickFixes) where

import Atom.Editor (TextEditor)
import Control.Monad.Eff (Eff)
import Data.Function.Eff (runEffFn3, EffFn3)
import Data.List
import IdePurescript.Atom.Build (AtomLintMessage)
import IdePurescript.Atom.Hooks.Linter (LinterInternal)
import Prelude (Unit)

foreign import showQuickFixesImpl :: forall eff. EffFn3 eff TextEditor LinterInternal (Array AtomLintMessage) Unit

-- EFF type
showQuickFixes :: forall eff. TextEditor -> LinterInternal -> Array AtomLintMessage -> Eff eff Unit
showQuickFixes = runEffFn3 showQuickFixesImpl
