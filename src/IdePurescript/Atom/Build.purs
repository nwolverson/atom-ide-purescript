module IdePurescript.Atom.Build where

import Prelude
import Atom.Atom (Atom, getAtom)
import Atom.Config (CONFIG, getConfig)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (readBoolean)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import IdePurescript.Build (BuildEff, BuildResult, Command(..), build)
import IdePurescript.PscErrors (Position, PscError(PscError))

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
  , suggestion :: { hasSuggestion :: Boolean, replacement :: String, range :: Array (Array Int) } -- not a linter field
  , errorCode :: String -- not a linter field
  }

data Result = Errors | Success

resultToString :: Result -> String
resultToString Errors = "errors"
resultToString Success = "success"

fixPosition :: Position -> Position
fixPosition pos@{startLine, startColumn, endLine, endColumn }
  | startLine == endLine && startColumn == endColumn = pos { endColumn = endColumn + 1}
  | otherwise = pos

type LintResult =
  { result :: String
  , messages :: Array AtomLintMessage
  }

linterBuild :: forall eff. { command :: String, args :: Array String, directory :: String } ->
  Aff (BuildEff (config :: CONFIG | eff)) BuildResult
linterBuild { command, args, directory } = do
  atom <- liftEff (getAtom :: Eff (BuildEff (config :: CONFIG | eff)) Atom)
  addNpmPath <- liftEff $ readBoolean <$> getConfig atom.config "ide-purescript.addNpmPath"
  build
    { command: Command command args
    , directory
    , useNpmDir: either (const false) id $ runExcept addNpmPath
    }

toLintResult :: BuildResult -> LintResult
toLintResult res =
  let warnings = result "Warning" <$> res.errors.warnings
      errors = result "Error" <$> res.errors.errors
  in {
    messages: errors <> warnings
  , result: resultToString $ if res.success then Success else Errors
  }
  where
  range :: Maybe Position -> Array (Array Int)
  range Nothing = []
  range (Just {startLine, startColumn, endLine, endColumn}) =
    [[startLine-1, startColumn-1], [endLine-1, endColumn-1]]

  result errorType (PscError {message,filename,position,errorLink,errorCode,suggestion}) =
    {
      "type": errorType
    , text: message
    , suggestion: replace suggestion
    , filePath: fromMaybe "" filename
    , range: range $ fixPosition <$> position
    , multiline: true
    , errorCode
    , trace: [
      {
        type: "Link"
      , html: "<a href=\"" <> errorLink <> "\">More info (wiki)</a>"
      }
    ]}
    where
    replace (Just { replacement, replaceRange }) =
      { replacement
      , hasSuggestion: true
      , range: range $ maybe (fixPosition <$> position) Just replaceRange
      }
    replace Nothing = { replacement: "", hasSuggestion: false, range: range Nothing }
