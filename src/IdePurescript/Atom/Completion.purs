module IdePurescript.Atom.Completion where

import Control.Monad.Aff (Aff)
import Data.Array (filter)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String (indexOf, contains)
import Data.String.Regex (Regex, noFlags, regex, test, match)
import Data.String.Regex (match, regex)
import IdePurescript.PscIde (getCompletion, eitherToErr)
import Prelude (return, map, ($), bind, (==), (/=), (||), (++))
import PscIde as P
import PscIde.Command as C
import PscIde

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  }

moduleRegex :: Regex
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
  return $ case list of
    (C.ModuleList lst) -> filter (\m -> indexOf prefix m == Just 0) lst

getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo
  } -> Aff (net :: P.NET | eff) (Array AtomSuggestion)
getSuggestions port { line, moduleInfo: { modules, getQualifiedModule } } =
  let moduleCompletion = indexOf "import" line == Just 0

  in case parsed of
    Just { mod, token } ->
      if moduleCompletion then do
        let prefix = getModuleName mod token
        completions <- getModuleSuggestions port prefix
        return $ map (modResult prefix) completions
      else do
        completions <- getCompletion port token mod moduleCompletion modules getQualifiedModule
        return $ map (result mod token) completions
    Nothing -> return []
  where
    getModuleName "" token  = token
    getModuleName mod token = mod ++ "." ++ token

    parsed =
      case match moduleRegex line of
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

    result qualifier prefix {"type": ty, identifier, "module": mod} =
      -- include both text and snippet as suggestions must be unique by text+snippet
      -- we want duplicates to disambiguate modules, but only insert the ident while
      -- triggering an import for the module
      { text: mod ++ "." ++ identifier
      , snippet: identifier
      , displayText: case suggestType of
          Type -> identifier
          _ -> identifier
      , type: suggestionTypeAtomValue suggestType
      , description: ty
      , replacementPrefix: prefix
      , rightLabel: mod
      , className: "purescript-suggestion"
      , addImport: Just { mod, identifier, qualifier: if qualifier == "" then Nothing else Just qualifier }
      }
      where
        suggestType =
          if contains "->" ty then Function
          else if test (regex "^[A-Z]" noFlags) identifier then Type
          else Value
