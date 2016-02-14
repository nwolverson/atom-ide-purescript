module IdePurescript.PscIde where

import Prelude (($), pure, bind, map, (<$>), return, (<<<))
import Data.Either (Either(Right, Left))
import Data.Maybe(Maybe(Nothing,Just))
import Data.Array((:), null, head)
import Control.Monad.Eff (Eff)
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception as Ex
import Control.Monad.Error.Class (throwError)
import Data.Nullable (toNullable)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Combinators ((.?))
import Data.String as S
import Data.String.Regex (replace, noFlags, regex)

import Control.Promise (Promise, fromAff)

result :: forall a eff. Aff (net :: P.NET | eff) (Either String a) -> Eff (net :: P.NET | eff) (Promise a)
result = fromAff <<< eitherToErr

eitherToErr :: forall a eff. Aff (net :: P.NET | eff) (Either String a) -> (Aff (net :: P.NET | eff) a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (Ex.error s)
    Right res -> return res

result' :: forall eff a b. (a -> b) ->  Aff (net :: P.NET | eff) (Either String a) -> Eff (net :: P.NET | eff) (Promise b)
result' f a = result ((f <$>) <$> a)

resultA :: forall eff a b. (a -> b) ->  Aff (net :: P.NET | eff) (Either String a) -> Aff (net :: P.NET | eff) b
resultA f a = eitherToErr ((f <$>) <$> a)


cwd :: forall eff. Eff (net :: P.NET | eff) (Promise String)
cwd = result' runMsg P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getImports' :: forall eff. String
  -> Aff (net :: P.NET | eff) (Array _)
getImports' s = resultA conv $ P.listImports s
  where
  conv (C.ImportList imps) = conv' <$> imps
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

getImports :: forall eff. String
  -> Eff (net :: P.NET | eff) (Promise (Array _))
getImports s = result' conv $ P.listImports s
  where
  conv (C.ImportList imps) = conv' <$> imps
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

getAvailableModules :: forall eff. Eff (net :: P.NET | eff) (Promise (Array String))
getAvailableModules = result' conv P.listAvailableModules
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

getTypeA :: forall eff. String -> String -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) String
getTypeA text modulePrefix unqualModules getQualifiedModule =
  resultA conv $ P.type' text $ moduleFilters mods
  where
    mods = moduleFilterModules modulePrefix unqualModules getQualifiedModule
    conv r = case head $ map convCompletion r of
              Just res -> abbrevType res.type
              Nothing -> ""

getType :: forall eff. String -> String -> Array String -> (String -> Array String)
  -> Eff (net :: P.NET | eff) (Promise String)
getType text modulePrefix unqualModules getQualifiedModule =
  result' conv $ P.type' text $ moduleFilters mods
  where
    mods = moduleFilterModules modulePrefix unqualModules getQualifiedModule
    conv r = case head $ map convCompletion r of
              Just res -> abbrevType res.type
              Nothing -> ""

getCompletion :: forall eff. String -> String -> Boolean -> Array String -> (String -> Array String)
  -> Aff (net :: P.NET | eff) (Array _)
getCompletion prefix modulePrefix moduleCompletion unqualModules getQualifiedModule =
  conv <$> (eitherToErr $ P.complete (C.PrefixFilter prefix : moduleFilters mods) Nothing)
  where
  mods = if moduleCompletion then [] else moduleFilterModules modulePrefix unqualModules getQualifiedModule
  conv = map convCompletion

convCompletion :: C.Completion -> { type :: String, identifier :: String }
convCompletion (C.Completion { type', identifier }) = { type: type', identifier }

loadDeps :: forall eff. String
  -> Eff (net :: P.NET | eff) (Promise String)
loadDeps main = result' runMsg $ P.load [] [main]

loadDepsA :: forall eff. String
  -> Aff (net :: P.NET | eff) String
loadDepsA main = resultA runMsg $ P.load [] [main]

getPursuitCompletion :: forall eff. String
  -> Eff (net :: P.NET | eff) (Promise (Array _))
getPursuitCompletion str = result' (map convPursuitCompletion) $ P.pursuitCompletion str

convPursuitCompletion :: C.PursuitCompletion -> { type :: String, identifier :: String, module :: String, package :: String }
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
--
-- getPursuitModuleCompletion :: forall eff. String
--   -> Eff (net :: P.NET | eff) (Promise (Array ModuleCompletion))
-- getPursuitModuleCompletion str = result' (map id) $ complete str
--   where
--   complete :: String -> P.Cmd (Array ModuleCompletion)
--   complete q = P.sendCommand (C.Pursuit C.Package q)


getPursuitModuleCompletion :: forall eff. String
  -> Aff (net :: P.NET | eff) (Array ModuleCompletion)
getPursuitModuleCompletion str = eitherToErr $ complete str
  where
  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand (C.Pursuit C.Package q)
