module IdePurescript.Modules (modulesState) where

import Prelude ((<$>), pure, ($), bind, map, (==), (<<<), (++), const)
import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Array (filter, singleton)
import Control.Monad.Aff (Aff)
import Data.Either (either)
import Control.Monad.Eff.Ref (writeRef, readRef, newRef, REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Data.String.Regex as R

import PscIde as P
import PscIde.Command as C

import Control.Promise as Promise
import Control.Promise (Promise)

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

modulesStateImpl = do
  stateRef <- newRef { main: Nothing, modules: [] }
  pure $
    {
      getQualModule: \s -> getQualModule s <$> readRef stateRef
    , getUnqualActiveModules: getUnqualActiveModules <$> readRef stateRef
    , loadModulesForFile: \f s -> do
        state <- getModulesForFile f s
        liftEff $ writeRef stateRef state
    , getMainModule: (\m -> fromMaybe "" m.main) <$> readRef stateRef
    }

modulesState :: forall eff. Eff (ref :: REF | eff) _
modulesState = do
  m <- modulesStateImpl
  pure
    {
      loadModules: \f s -> Promise.fromAff $ m.loadModulesForFile f s
    , getUnqualActiveModules: m.getUnqualActiveModules
    , getQualModule: m.getQualModule
    , getMainModule: m.getMainModule
    , getMainModuleForFile: fromMaybe "" <<< getMainModule
    }
