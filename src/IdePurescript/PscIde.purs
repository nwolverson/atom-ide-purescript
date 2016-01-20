module IdePurescript.PscIde where

import Prelude (id, map, ($), pure, bind, (<$>), (<<<), return)
import Data.Either (Either(Right, Left))
import Data.Maybe(Maybe(Nothing))
import Data.Array((:))
import Control.Monad.Eff (Eff)
import PscIde as P
import PscIde.Command as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception as Ex
import IdePurescript.Promise (Promise, toPromise)
import Control.Monad.Error.Class (throwError)
import Data.Nullable (toNullable)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Combinators ((.?))

result :: forall a eff. Aff (net :: P.NET | eff) (Either String a) -> Eff (net :: P.NET | eff) Promise
result = toPromise <<< eitherToErr
  where
  -- throw error for Either
  eitherToErr :: Aff (net :: P.NET | eff) (Either String a) -> (Aff (net :: P.NET | eff) a)
  eitherToErr c = do
    r <- c
    case r of
      Left s -> throwError (Ex.error s)
      Right res -> return res

result' :: forall eff a b. (a -> b) ->  Aff (net :: P.NET | eff) (Either String a) -> Eff (net :: P.NET | eff) Promise
result' f a = result ((f <$>) <$> a)

cwd :: forall eff. Eff (net :: P.NET | eff) Promise
cwd = result' runMsg P.cwd

runMsg :: C.Message -> String
runMsg (C.Message m) = m

getImports :: forall eff. String
  -> Eff (net :: P.NET | eff) Promise
getImports s = result' conv $ P.listImports s
  where
  conv (C.ImportList imps) = conv' <$> imps
  conv' (C.Import {moduleName, qualifier}) = {
    "module": moduleName,
    qualifier: toNullable qualifier
  }

moduleFilters :: Array String -> Array C.Filter
moduleFilters [] = []
moduleFilters modules = [ C.ModuleFilter modules ]

getType :: forall eff. String -> Array String
  -> Eff (net :: P.NET | eff) Promise
getType text modules =
  result' (map convCompletion) $ P.type' text $ moduleFilters modules

getCompletion :: forall eff. String -> Array String
  -> Eff (net :: P.NET | eff) Promise
getCompletion prefix modules = result' (map convCompletion) $
    P.complete (C.PrefixFilter prefix : moduleFilters modules) Nothing

convCompletion :: C.Completion -> { type :: String, identifier :: String }
convCompletion (C.Completion { type', identifier }) = { type: type', identifier }

loadDeps :: forall eff. String
  -> Eff (net :: P.NET | eff) Promise
loadDeps main = result' runMsg $ P.load [] [main]

getPursuitCompletion :: forall eff. String
  -> Eff (net :: P.NET | eff) Promise
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

getPursuitModuleCompletion :: forall eff. String
  -> Eff (net :: P.NET | eff) Promise
getPursuitModuleCompletion str = result' (map id) $ complete str
  where
  complete :: String -> P.Cmd (Array ModuleCompletion)
  complete q = P.sendCommand (C.Pursuit C.Package q)
