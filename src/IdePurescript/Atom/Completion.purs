module IdePurescript.Atom.Completion where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (filter)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Either (Either)
import Data.String (indexOf, contains)
import Data.String.Regex (Regex, noFlags, regex)
import IdePurescript.PscIde (getCompletion, eitherToErr)
import IdePurescript.Regex (test', match')
import PscIde as P
import PscIde.Command as C

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  , mainModule :: Maybe String
  }

moduleRegex :: Either String Regex
moduleRegex = regex """(?:^|[^A-Za-z_.])(?:((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.)?([a-zA-Z][a-zA-Z0-9_']*)?$""" noFlags

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
    (C.ModuleList lst) -> filter (\m -> indexOf prefix m == Just 0) lst

getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo
  } -> Aff (net :: P.NET | eff) (Array AtomSuggestion)
getSuggestions port { line, moduleInfo: { modules, getQualifiedModule, mainModule } } =
  let moduleCompletion = indexOf "import" line == Just 0

  in case parsed of
    Just { mod, token } ->
      if moduleCompletion then do
        let prefix = getModuleName mod token
        completions <- getModuleSuggestions port prefix
        pure $ map (modResult prefix) completions
      else do
        completions <- getCompletion port token mainModule mod moduleCompletion modules getQualifiedModule
        pure $ map (result mod token) completions
    Nothing -> pure []
  where
    getModuleName "" token  = token
    getModuleName mod token = mod <> "." <> token

    parsed =
      case match' moduleRegex line of
        Just [ Just _, mod, tok ] | mod /= Nothing || tok /= Nothing ->
          Just { mod: fromMaybe "" mod , token: fromMaybe "" tok}
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
      , addImport: Just { mod, identifier, qualifier: if qualifier == "" then Nothing else Just qualifier }
      }
      where
        suggestType =
          if contains "->" type' then Function
          else if test' (regex "^[A-Z]" noFlags) identifier then Type
          else Value
