module IdePurescript.Atom.Completion where

import Prelude
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Data.Array (filter)
import Data.Either (Either)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String (Pattern(Pattern), indexOf, contains)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.PscIde (getCompletion, eitherToErr)
import IdePurescript.Regex (test', match')

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  , mainModule :: Maybe String
  }

modulePart :: String
modulePart = """((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))"""

identPart :: String
identPart = "([a-zA-Z_][a-zA-Z0-9_']*)"

moduleRegex :: Either String Regex
moduleRegex = regex ("(?:^|[^A-Za-z_.])(?:" <> modulePart <> """\.)?""" <> identPart <> "?$") noFlags

explicitImportRegex :: Either String Regex
explicitImportRegex = regex ("""^import\s+""" <> modulePart <> """\s+\([^)]*?""" <> identPart <> "$") noFlags

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

data SuggestionType = Module | Type | Function | Value

getModuleSuggestions :: forall eff. Int -> String -> Aff (net :: P.NET | eff) (Array String)
getModuleSuggestions port prefix = do
  list <- eitherToErr $ P.listAvailableModules port
  pure $ case list of
    (C.ModuleList lst) -> filter (\m -> indexOf (Pattern prefix) m == Just 0) lst


getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo
  } -> Aff (net :: P.NET | eff) (Array AtomSuggestion)
getSuggestions port { line, moduleInfo: { modules, getQualifiedModule, mainModule } } =
  if moduleExplicit then
    case match' explicitImportRegex line of
      Just [ Just _, Just mod, Just token ] -> do
        completions <- getCompletion port token mainModule Nothing [ mod ] getQualifiedModule
        pure $ map (result (Just mod) token) completions
      _ -> pure []
  else
    case parsed of
      Just { mod, token } ->
        if moduleCompletion then do
          let prefix = getModuleName (fromMaybe "" mod) token
          completions <- getModuleSuggestions port prefix
          pure $ map (modResult prefix) completions
        else do
          completions <- getCompletion port token mainModule mod modules getQualifiedModule
          pure $ map (result mod token) completions
      Nothing -> pure []
    where
    getModuleName "" token  = token
    getModuleName mod token = mod <> "." <> token

    isImport = indexOf (Pattern "import") line == Just 0
    hasBracket = indexOf (Pattern "(") line /= Nothing
    moduleCompletion = isImport && not hasBracket
    moduleExplicit = isImport && hasBracket

    parsed = case match' moduleRegex line of
        Just [ Just _, mod, tok ] | mod /= Nothing || tok /= Nothing ->
          Just { mod, token: fromMaybe "" tok}
        _ -> Nothing

    suggestionTypeAtomValue s = case s of
      Module -> "import"
      Type -> "type"
      Function -> "function"
      Value -> "value"

    modResult prefix moduleName =
      { text: moduleName
      , snippet: moduleName
      , displayText: moduleName
      , type: suggestionTypeAtomValue Module
      , description: ""
      , replacementPrefix: prefix
      , rightLabel: ""
      , className: "purescript-suggestion"
      , addImport: Nothing
      }

    result qualifier prefix (C.TypeInfo {type', identifier, module': mod}) =
      -- include both text and snippet as suggestions must be unique by text+snippet
      -- we want duplicates to disambiguate modules, but only insert the ident while
      -- triggering an import for the module
      { text: mod <> "." <> identifier
      , snippet: identifier
      , displayText: case suggestType of
          Type -> identifier
          _ -> identifier
      , type: suggestionTypeAtomValue suggestType
      , description: type'
      , replacementPrefix: prefix
      , rightLabel: mod
      , className: "purescript-suggestion"
      , addImport: Just { mod, identifier, qualifier }
      }
      where
        suggestType =
          if contains (Pattern "->") type' then Function
          else if test' (regex "^[A-Z]" noFlags) identifier then Type
          else Value
