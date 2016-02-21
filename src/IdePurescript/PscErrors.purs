module IdePurescript.PscErrors where

import Prelude ((<*>), (<$>), map, ($), pure, (>>=), (<<<))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Argonaut.Core (JObject, toObject)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Traversable (traverse)
import Control.Bind ((<=<))

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

type PscResult =
  { warnings :: Array PscError
  , errors :: Array PscError
  }

type PscError =
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  , errorLink :: String
  , suggestion :: Maybe String
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

parsePscOutput :: String -> Either String PscResult
parsePscOutput = maybe (Left "not object") parsePscResult <<< toObject <=< jsonParser

parsePscResult :: JObject -> Either String PscResult
parsePscResult obj =
  { warnings: _
  , errors: _
  } <$> (obj .? "warnings" >>= traverse parsePscError)
    <*> (obj .? "errors" >>= traverse parsePscError)

parsePscError :: JObject -> Either String PscError
parsePscError obj =
  { moduleName: _
  , errorCode: _
  , message: _
  , filename: _
  , position: _
  , errorLink: _
  , suggestion: _
  } <$> obj .? "moduleName"
    <*> obj .? "errorCode"
    <*> obj .? "message"
    <*> obj .? "filename"
    <*> (obj .? "position" >>= parsePosition)
    <*> obj .? "errorLink"
    <*> (obj .? "suggestion" >>= parseSuggestion)

parsePosition :: Maybe JObject -> Either String (Maybe Position)
parsePosition =
  maybe (pure Nothing) \obj -> map Just $
    { startLine: _
    , startColumn: _
    , endLine: _
    , endColumn: _
    } <$> obj .? "startLine"
      <*> obj .? "startColumn"
      <*> obj .? "endLine"
      <*> obj .? "endColumn"

parseSuggestion :: Maybe JObject -> Either String (Maybe String)
parseSuggestion =
  maybe (pure Nothing) \obj -> obj .? "replacement"
