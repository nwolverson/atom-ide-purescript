module IdePurescript.Atom.Pursuit (pursuitSearch, pursuitSearchModules, SearchResult) where

import Prelude (Unit)
import Data.Function.Eff (EffFn1, EffFn2, mkEffFn1, runEffFn2, runEffFn1)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise)

foreign import pursuitSearchImpl :: forall eff. EffFn1 eff (EffFn1 eff String (Promise (Array SearchResult))) Unit

type SearchResult = { module :: String, package :: String, type:: String, identifier :: String }

pursuitSearch :: forall eff. (String -> Eff eff (Promise (Array SearchResult))) -> Eff eff Unit
pursuitSearch searchFn = runEffFn1 pursuitSearchImpl (mkEffFn1 searchFn)

type ModuleSearchResult = { module :: String, package :: String }

foreign import pursuitSearchModulesImpl :: forall eff. EffFn2 eff (EffFn1 eff String (Promise (Array ModuleSearchResult))) (EffFn1 eff Unit Unit) Unit

pursuitSearchModules :: forall eff. (String -> Eff eff (Promise (Array ModuleSearchResult))) -> (Unit -> Eff eff Unit) -> Eff eff Unit
pursuitSearchModules searchFn importFn = runEffFn2 pursuitSearchModulesImpl (mkEffFn1 searchFn) (mkEffFn1 importFn)
