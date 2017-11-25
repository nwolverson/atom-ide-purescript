module IdePurescript.Atom.Hooks.Linter where

import Prelude (Unit)

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1)
import Atom.Range (Range, getEnd, mkRange)
import Data.Nullable (Nullable, toNullable)

foreign import data LinterRegistry :: Type

-- | Referred to in linter documentation as IndieDelegate
foreign import data LinterIndie :: Type

foreign import data EditorLinter :: Type
foreign import data LINTER :: Effect

type AtomLintMessage eff =
  { severity :: String -- error | warning | info
  , excerpt :: String
  , description :: String
  , location ::
    { file :: String
    , position:: Range
    }
  , url :: String
  , icon :: Nullable String
  , solutions :: Array (Solution eff)
  }

type Solution eff =
  { title :: String
  , position :: Range
  , priority :: Int

  -- Either apply or replaceWith required
  , apply :: Nullable (Eff eff Unit)
  , currentText :: Nullable String
  , replaceWith :: Nullable String
  }

-- | Argument type of the consumeIndie entry-point function
type RegisterIndie = forall eff. EffFn1 (linter:: LINTER | eff) { name :: String } LinterIndie

foreign import clearMessages :: forall eff. LinterIndie -> Eff (linter :: LINTER | eff) Unit

-- | Set messages for a particular file, leaving others unchanged
foreign import setMessages :: forall eff. LinterIndie -> String -> Array (AtomLintMessage eff) -> Eff (linter :: LINTER | eff) Unit

-- | Set all messages, clearing any existing
foreign import setAllMessages :: forall eff. LinterIndie -> Array (AtomLintMessage eff) -> Eff (linter :: LINTER | eff) Unit

foreign import getMessages :: forall eff. EditorLinter -> Eff (linter :: LINTER | eff) (Array (AtomLintMessage eff))
