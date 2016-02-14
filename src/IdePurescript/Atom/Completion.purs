module IdePurescript.Atom.Completion where

import Prelude (return, map, ($), bind, (==), (/=), (||), (++))
import Control.Monad.Aff (Aff)
import Data.Array (filter)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String (indexOf, contains)
import Data.String.Regex (match, regex)
import Data.String.Regex (Regex, noFlags, regex, test, match)
import IdePurescript.PscIde (getCompletion, eitherToErr)
import PscIde as P
import PscIde.Command as C

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  }

moduleRegex :: Regex
moduleRegex = regex """(?:^|[^A-Za-z_.])(?:((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.)?([a-zA-Z][a-zA-Z0-9_']*)?$""" noFlags

type AtomSuggestion =
  { text :: String
  , displayText :: String
  , "type" :: String
  , description :: String
  , replacementPrefix :: String
  }

data SuggestionType = Module | Type | Function | Value

getModuleSuggestions :: forall eff. String -> Aff (net :: P.NET | eff) (Array String)
getModuleSuggestions prefix = do
  list <- eitherToErr $ P.listAvailableModules
  return $ case list of
    (C.ModuleList lst) -> filter (\m -> indexOf prefix m == Just 0) lst

getSuggestions :: forall eff. {
    line :: String,
    moduleInfo :: ModuleInfo
  } -> Aff (net :: P.NET | eff) (Array AtomSuggestion)
getSuggestions { line, moduleInfo: { modules, getQualifiedModule } } =
  let moduleCompletion = indexOf "import" line == Just 0

  in case parsed of
    Just { mod, token } ->
      if moduleCompletion then do
        let prefix = getModuleName mod token
        completions <- getModuleSuggestions prefix
        return $ map (modResult prefix) completions
      else do
        completions <- getCompletion token mod moduleCompletion modules getQualifiedModule
        return $ map (result token) completions
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
      , displayText: moduleName
      , type: suggestionTypeAtomValue Module
      , description: ""
      , replacementPrefix: prefix
      }

    result prefix {type: ty, identifier} =
      { text: identifier
      , displayText: case suggestType of
          Type -> identifier ++ " " ++ ty
          _ -> identifier ++ ": " ++ ty
      , type: suggestionTypeAtomValue suggestType
      , description: ty
      , replacementPrefix: prefix
      }
      where
        suggestType =
          if contains "->" ty then Function
          else if test (regex "^[A-Z]" noFlags) identifier then Type
          else Value
