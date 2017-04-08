module IdePurescript.Atom.Build where

import Prelude
import Data.String as Str
import Atom.Atom (Atom, getAtom)
import Atom.Config (CONFIG, getConfig)
import Atom.Editor (TextEditor, getPath, getTextInRange)
import Atom.Point (Point, getColumn, getRow, mkPoint)
import Atom.Range (Range, getEnd, mkRange)
import Atom.Types (EDITOR)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Except (runExcept)
import Data.Array (filter, null)
import Data.Either (either)
import Data.Foreign (readBoolean)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.String.Regex (regex) as Regex
import Data.String.Regex.Flags (global, noFlags) as Regex
import Data.Traversable (traverse)
import IdePurescript.Atom.Assist (TypoEff, fixTypo)
import IdePurescript.Build (BuildEff, BuildResult, Command(..), build)
import IdePurescript.Modules (State)
import IdePurescript.PscErrors (Position, PscError(PscError), PscSuggestion)
import IdePurescript.QuickFix (getTitle, isUnknownToken)
import IdePurescript.Regex (replace', test')

type Solution eff =
  { title :: String
  , position :: Range
  , priority :: Int

  -- Either apply or replaceWith required
  , apply :: Nullable (Eff eff Unit)
  , currentText :: Nullable String
  , replaceWith :: Nullable String
  }

type AtomLintMessage eff =
  { severity :: String -- error | warning | info
  , excerpt :: String
  , location ::
    { file :: String
    , position:: Range
    }
  , url :: String
  , icon :: Nullable String
  , solutions :: Array (Solution eff)
  }

data Result = Errors | Success

resultToString :: Result -> String
resultToString Errors = "errors"
resultToString Success = "success"

fixPosition :: Position -> Position
fixPosition pos@{startLine, startColumn, endLine, endColumn }
  | startLine == endLine && startColumn == endColumn = pos { endColumn = endColumn + 1}
  | otherwise = pos

type LintResult eff =
  { result :: String
  , messages :: Array (AtomLintMessage eff)
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

toLintResult :: forall eff eff'. (PscError -> Boolean) -> Maybe TextEditor -> Ref State -> Maybe Int -> BuildResult -> Eff (editor :: EDITOR | eff') (LintResult (TypoEff eff))
toLintResult resultFilter editor modulesStateRef port res = do
  warnings <- traverse (result "warning") (filter resultFilter res.errors.warnings)
  errors <- traverse (result "error") (filter resultFilter res.errors.errors)
  pure {
    messages: errors <> warnings
  , result: resultToString $ if res.success then Success else Errors
  }
  where
  range :: Maybe Position -> Range
  range Nothing = mkRange (mkPoint 0 0) (mkPoint 0 0)
  range (Just {startLine, startColumn, endLine, endColumn}) =
    mkRange (mkPoint (startLine-1) (startColumn-1)) (mkPoint (endLine-1) (endColumn-1))

  result :: forall eff1. String -> PscError -> Eff ( editor :: EDITOR | eff1) (AtomLintMessage (TypoEff eff))
  result errorType (PscError {message,filename,position,errorLink,errorCode,suggestion}) = do
    correctEditor <- maybe (pure false) (\e -> (==) filename <$> getPath e) editor
    let editor' = if correctEditor then editor else Nothing
    rep <- replace editor' suggestion
    let solutions = rep <> fixes port
    pure
      {
        severity: errorType
      , excerpt: message
      , location:
        { file: fromMaybe "" filename
        , position: range $ fixPosition <$> position
        }
      , url: errorLink
      , icon: toNullable $ if null solutions then Nothing else Just "zap"
      , solutions
      }
    where
    replace :: forall eff''. Maybe TextEditor -> Maybe PscSuggestion -> Eff ( editor :: EDITOR | eff'') (Array (Solution (TypoEff eff)))
    replace (Just editor') (Just { replacement, replaceRange }) = do
      let resRange = range $ maybe (fixPosition <$> position) Just replaceRange
      text <- getTextInRange editor' resRange
      extraText <- getTextInRange editor' (mkRange (getEnd resRange) (bumpPoint 0 10 $ getEnd resRange))
      let trailingNewline = test' (Regex.regex "\n\\s+$" Regex.noFlags) replacement
          addNewline = trailingNewline && (not $ Str.null extraText)
          replacement' = Str.trim $ replace' (Regex.regex "\\s+\n" Regex.global) "\n" replacement
      pure [
        { title: getTitle errorCode
        , position: resRange
        , priority: 0
        , apply: toNullable Nothing
        , replaceWith: toNullable $ Just replacement'
        , currentText: toNullable $ Just text
        }
      ]
    replace _ _ = pure []

    bumpPoint :: Int -> Int -> Point -> Point
    bumpPoint r c p = mkPoint (getRow p + r) (getColumn p + c)

    fixes (Just port') | isUnknownToken errorCode =
      [
        { title: "Fix typo"
        , position: range $ (fixPosition <$> position)
        , priority: 0
        , apply: toNullable $ Just $ fixTypo modulesStateRef port'
        , replaceWith: toNullable Nothing
        , currentText: toNullable Nothing
        }

      ]
    fixes _ | otherwise = []
