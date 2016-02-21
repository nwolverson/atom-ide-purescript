module IdePurescript.PscIde (getCompletion, cwd, loadDeps, getType, eitherToErr
  , getPursuitModuleCompletion, getPursuitCompletion, getAvailableModules, SearchResult, ModuleSearchResult) where

import Prelude (map, ($), pure, bind, (<$>), return)
import Data.Either (Either(Right, Left))
import Data.Maybe(Maybe(Nothing,Just))
import Data.Array((:), null, head)
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception as Ex
import Control.Monad.Error.Class (throwError)
import Data.Nullable (toNullable, Nullable)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Combinators ((.?))
import Data.String as S
import Data.String.Regex (replace, noFlags, regex)

eitherToErr :: forall a eff. Aff (net :: P.NET | eff) (Either String a) -> (Aff (net :: P.NET | eff) a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (Ex.error s)
    Right res -> return res

result :: forall eff a b. (a -> b) ->  Aff (net :: P.NET | eff) (Either String a) -> Aff (net :: P.NET | eff) b
result f a = eitherToErr ((f <$>) <$> a)

cwd :: forall eff. Aff (net :: P.NET | eff) String
cwd = result runMsg P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getImports' :: forall eff. String
  -> Aff (net :: P.NET | eff) (Array { module :: String, qualifier :: Nullable String })
getImports' s = result conv $ P.listImports s
  where
  conv (C.ImportList imps) = conv' <$> imps
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

getAvailableModules :: forall eff. Aff (net :: P.NET | eff) (Array String)
getAvailableModules = result conv P.listAvailableModules
  where
  conv (C.ModuleList modules) = modules

moduleFilterModules :: String -> Array String -> (String -> Array String) -> Array String
moduleFilterModules modulePrefix unqualModules getQualifiedModule =
  if S.null modulePrefix then
    unqualModules
  else if S.contains "." modulePrefix then
    [ modulePrefix ]
  else
    let mods = getQualifiedModule modulePrefix in
    if null mods then
      [ modulePrefix ]
    else
      mods

moduleFilters :: Array String -> Array C.Filter
moduleFilters [] = []
moduleFilters modules = [ C.ModuleFilter modules ]


abbrevType :: String -> String
abbrevType = replace r "$1"
  where r = regex """(?:\w+\.)+(\w+)""" $ noFlags { global = true}

getType :: forall eff. String -> String -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) String
getType text modulePrefix unqualModules getQualifiedModule =
  result conv $ P.type' text $ moduleFilters mods
  where
    mods = moduleFilterModules modulePrefix unqualModules getQualifiedModule
    conv r = case head $ map convCompletion r of
              Just res -> abbrevType res.type
              Nothing -> ""

type CompletionResult = {type :: String, identifier :: String}

getCompletion :: forall eff. String -> String -> Boolean -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) (Array CompletionResult)
getCompletion prefix modulePrefix moduleCompletion unqualModules getQualifiedModule =
  conv <$> (eitherToErr $ P.complete (C.PrefixFilter prefix : moduleFilters mods) Nothing)
  where
  mods = if moduleCompletion then [] else moduleFilterModules modulePrefix unqualModules getQualifiedModule
  conv = map convCompletion

convCompletion :: C.Completion -> CompletionResult
convCompletion (C.Completion { type', identifier }) = { type: type', identifier }

loadDeps :: forall eff. String
  -> Aff (net :: P.NET | eff) String
loadDeps main = result runMsg $ P.load [] [main]

type SearchResult = { module :: String, package :: String, type:: String, identifier :: String }

getPursuitCompletion :: forall eff. String -> Aff (net :: P.NET | eff) (Array SearchResult)
getPursuitCompletion str = result (map convPursuitCompletion) $ P.pursuitCompletion str

convPursuitCompletion :: C.PursuitCompletion -> SearchResult
convPursuitCompletion (C.PursuitCompletion { identifier, type', module', package })
  = { identifier, package, type: type', module: module' }

data ModuleCompletion = ModuleCompletion {
  module' :: String,
  package :: String
}

instance decodeModuleCompletion :: DecodeJson ModuleCompletion where
  decodeJson json = do
    o <- decodeJson json
    module' <- o .? "module"
    package <- o .? "package"
    pure (ModuleCompletion {
      module': module',
      package: package
      })

type ModuleSearchResult = { module :: String, package :: String }

getPursuitModuleCompletion :: forall eff. String
  -> Aff (net :: P.NET | eff) (Array ModuleSearchResult)
getPursuitModuleCompletion str = result (map convPursuitModuleCompletion) $ complete str
  where

  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand (C.Pursuit C.Package q)

  convPursuitModuleCompletion :: ModuleCompletion -> ModuleSearchResult
  convPursuitModuleCompletion (ModuleCompletion { module', package })
    = { package, module: module' }
