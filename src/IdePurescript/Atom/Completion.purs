module IdePurescript.Atom.Completion where

import Prelude
import IdePurescript.Completion as C
import PscIde as P
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Just, Nothing))
import IdePurescript.Completion (ModuleInfo, SuggestionResult(..), SuggestionType(..))

type AtomSuggestion =
  { text :: String
  , snippet :: String
  , displayText :: String
  , "type" :: String
  , description :: String
  , replacementPrefix :: String
  , rightLabel :: String
  , className :: String

  , addImport :: Maybe AddImport -- Not Atom suggestion property
  }

type AddImport =
  { mod :: String
  , identifier :: String
  , qualifier :: Maybe String
  }

getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo,
    groupCompletions :: Boolean,
    maxResults :: Maybe Int,
    preferredModules :: Array String
  } -> Aff (net :: P.NET | eff) (Array AtomSuggestion)
getSuggestions port info = do
  res <- C.getSuggestions port info
  pure $ convert <$> res

  where
    convert (ModuleSuggestion { text, suggestType, prefix }) =
      { text
      , snippet: text
      , displayText: text
      , "type": suggestionTypeAtomValue suggestType
      , description: ""
      , replacementPrefix: prefix
      , rightLabel: ""
      , className: "purescript-suggestion"
      , addImport: Nothing
      }
    convert (IdentSuggestion { origMod, exportMod, identifier, qualifier, suggestType, prefix, valueType }) =
    -- include both text and snippet as suggestions must be unique by text+snippet
    -- we want duplicates to disambiguate modules, but only insert the ident while
    -- triggering an import for the module
      { text: exportMod <> "." <> identifier
      , snippet: identifier
      , displayText: identifier
      , "type": suggestionTypeAtomValue suggestType
      , description: valueType
      , replacementPrefix: prefix
      , rightLabel: exportMod
      , className: "purescript-suggestion"
      , addImport: Just { mod: exportMod, identifier, qualifier }
      }

    suggestionTypeAtomValue s = case s of
      Module -> "import"
      Type -> "type"
      Function -> "function"
      Value -> "value"
