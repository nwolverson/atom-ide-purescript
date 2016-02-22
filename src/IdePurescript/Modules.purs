module IdePurescript.Modules (
    Module
  , initialModulesState
  , State
  , getMainModule
  , getModulesForFile
  , getUnqualActiveModules
  , getQualModule
  , findImportInsertPos
  ) where

import Prelude ((+), ($), map, (==), (<<<), (++), pure, const, bind)
import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Array (filter, singleton, findLastIndex)
import Control.Monad.Aff (Aff)
import Data.Either (either)
import Data.String (split)
import Data.String.Regex as R

import PscIde as P
import PscIde.Command as C

data Module = Unqualified String | Qualified String String

getModuleName :: Module -> String
getModuleName (Qualified _ m) = m
getModuleName (Unqualified m) = m

type State =
  { main :: Maybe String
  , modules :: Array Module
  }

type Path = String

getMainModule :: String -> Maybe String
getMainModule text =
  case R.match regex text of
    Just [_, Just m] -> Just m
    _ -> Nothing
  where
  regex = R.regex """module\s+([\w.]+)""" $ R.noFlags { multiline = true }

getModulesForFile :: forall eff. Path -> String -> Aff (net :: P.NET | eff) State
getModulesForFile file fullText = do
  imports <- P.listImports file
  let modules = either (const []) (\(C.ImportList l) -> map mod l) imports
  let main = getMainModule fullText
  pure { main, modules }
  where
  mod (C.Import { moduleName, qualifier: Nothing }) = Unqualified moduleName
  mod (C.Import { moduleName, qualifier: Just qual }) = Qualified qual moduleName

getUnqualActiveModules :: State -> Array String
getUnqualActiveModules {modules, main} =
  map getModuleName $ maybe [] (singleton <<< Unqualified) main ++ modules

getQualModule :: String -> State -> Array String
getQualModule qualifier {modules} =
  map getModuleName $ filter (qual qualifier) modules
  where
  qual q (Qualified q' _) = q == q'
  qual _ _ = false

initialModulesState :: State
initialModulesState =  { main: Nothing, modules: [] }

findImportInsertPos :: String -> Int
findImportInsertPos text =
  let regex = R.regex """^(module|import) [A-Z][^(]*($|\([^()]*\))""" R.noFlags
      lines = split "\n" text
      res = fromMaybe 0 $ findLastIndex (R.test regex) lines
  in res+1
