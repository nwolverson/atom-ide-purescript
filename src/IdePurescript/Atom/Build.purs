module IdePurescript.Atom.Build where

import Prelude (($), (++), pure, (<$>), bind, (-))
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import IdePurescript.Build (Command(Command), build)
import IdePurescript.PscErrors (Position)

-- This is really the same type but I'm using different fields
type AtomLintTraceMessage =
  { "type" :: String
  , html :: String
  }

type AtomLintMessage =
  { "type" :: String
  , text :: String
  , filePath :: String
  , range :: Array (Array Int)
  , multiline :: Boolean
  , trace :: Array AtomLintTraceMessage
  , suggestion :: { hasSuggestion :: Boolean, replacement :: String } -- not a linter field
  , errorCode :: String -- not a linter field
  }

data Result = Errors | Success

resultToString :: Result -> String
resultToString Errors = "errors"
resultToString Success = "success"

type LintResult =
  { result :: String
  , messages :: Array AtomLintMessage
  }

linterBuild :: { command :: String, args :: Array String, directory :: String }
  -> Aff _ LintResult
linterBuild {command,args,directory} = do
  res <- build { command: Command command args, directory }
  let warnings = result "Warning" <$> res.errors.warnings
      errors = result "Error" <$> res.errors.errors
  pure {
    messages: errors ++ warnings
  , result: resultToString $ if res.success then Success else Errors
  }
  where
  range :: Maybe Position -> Array (Array Int)
  range Nothing = []
  range (Just {startLine, startColumn, endLine, endColumn}) =
    [[startLine-1, startColumn-1], [endLine-1, endColumn-1]]

  result errorType {message,filename,position,errorLink,errorCode,suggestion} =
    {
      "type": errorType
    , text: message
    , suggestion: maybe { replacement: "", hasSuggestion: false } { replacement: _, hasSuggestion: true } suggestion
    , filePath: fromMaybe "" filename
    , range: range position
    , multiline: true -- /\n/.test(err.message)
    , errorCode
    , trace: [
      {
        type: "Link"
      , html: "<a href=\"" ++ errorLink ++ "\">More info (wiki)</a>"
      }
    ]}
