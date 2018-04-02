module IdePurescript.Atom.Config where

import Prelude

import Atom.Atom (getAtom)
import Atom.Config (CONFIG, getConfig)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe)
import Data.Bifunctor (rmap)
import Data.Either (either, hush)
import Data.Foreign (F, Foreign, readArray, readBoolean, readInt, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.StrMap (fromFoldable)
import Data.Traversable (for, for_, sequence, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Node.Platform (Platform(..))
import Node.Process as P

defaultSrcGlob :: Array String
defaultSrcGlob = ["src/**/*.purs", "bower_components/**/*.purs"]

getConfigOption :: forall eff a. (Foreign -> F a) -> String -> Eff (config :: CONFIG | eff ) (Maybe a)
getConfigOption readF key = do
  atom <- getAtom
  rawValue <- getConfig atom.config ("ide-purescript." <> key)
  pure $ either (const Nothing) Just $ runExcept $ readF rawValue

getSrcGlob :: forall eff. Eff (config :: CONFIG | eff) (Array String)
getSrcGlob = do
  atom <- getAtom
  srcGlob <- liftEff $ runExcept <$> readArray <$> getConfig atom.config "ide-purescript.pscSourceGlob"
  let srcGlob' = rmap (mapMaybe $ (either (const Nothing) Just) <<< runExcept <<< readString) $ srcGlob
  pure $ either (const defaultSrcGlob) id $ srcGlob'

getFastRebuild :: forall eff. Eff (config :: CONFIG | eff) Boolean
getFastRebuild = fromMaybe true <$> getConfigOption readBoolean "fastRebuild"

usePursDefault :: Boolean
usePursDefault = true

usePurs :: forall eff. Eff (config :: CONFIG | eff) Boolean
usePurs = fromMaybe usePursDefault <$> getConfigOption readBoolean "useCombinedExe"

pscIdeServerExe :: forall eff. Eff (config :: CONFIG | eff) String
pscIdeServerExe = fromMaybe "psc-ide-server" <$> getConfigOption readString "pscIdeServerExe"

pursExe :: forall eff. Eff (config :: CONFIG | eff) String
pursExe = fromMaybe "purs" <$> getConfigOption readString "pursExe"

-- | Combined effect of useCombinedExe + (pscIdeServerExe | pursExe)
effectiveServerExe :: forall eff. Eff (config :: CONFIG | eff) String
effectiveServerExe =
  usePurs >>= if _ then pursExe else pscIdeServerExe

addNpmPath :: forall eff. Eff (config :: CONFIG | eff) Boolean
addNpmPath = fromMaybe false <$> getConfigOption readBoolean "addNpmPath"


autoCompleteAllModules :: forall eff. Eff (config :: CONFIG | eff) Boolean
autoCompleteAllModules = fromMaybe false <$> getConfigOption readBoolean "autocomplete.allModules"

autoCompleteGrouped :: forall eff. Eff (config :: CONFIG | eff) Boolean
autoCompleteGrouped = fromMaybe false <$> getConfigOption readBoolean "autocomplete.grouped"

autoCompleteLimit :: forall eff. Eff (config :: CONFIG | eff) (Maybe Int)
autoCompleteLimit =  getConfigOption readInt "autocomplete.limit"

autoCompletePreferredModules :: forall eff. Eff (config :: CONFIG | eff) (Array String)
autoCompletePreferredModules = fromMaybe [] <$> getConfigOption readA "autocomplete.preferredModules"
  where readA = readArray >=> traverse readString

pulpCmd :: String
pulpCmd = if P.platform == Just Win32 then "pulp.cmd" else "pulp"

-- | Convert a atom-ide-purescript config object to one suitable for sending to language server
translateConfig :: Foreign -> Foreign
translateConfig config = either (const $ toForeign {}) id $ runExcept do
  let unchanged = [ "pursExe", "useCombinedExe", "pscIdeServerExe", "addNpmPath", "buildCommand", "fastRebuild", "censorWarnings", "editorMode", "polling", "pscIdelogLevel", "autoStartPscIde", "addPscPackageSources", "pscIdePort" ]
  unchangedOpts <- for unchanged (\p -> Tuple p <$> config ! p)
  autocomplete <- config ! "autocomplete"

  autocompleteOpts <- sequence
    [ Tuple "autocompleteAddImport" <$> autocomplete ! "addImport"
    , Tuple "autocompleteAllModules" <$> autocomplete ! "allModules"
    , Tuple "autocompleteLimit" <$> autocomplete ! "limit"
    , Tuple "autocompleteGrouped" <$> autocomplete ! "grouped"
    , Tuple "importsPreferredModules" <$> autocomplete ! "preferredModules"
    ]
  renamedOpts <- sequence
    [ Tuple "sourceGlobs" <$> config ! "pscSourceGlob"
    ]
  -- Unused:
  -- autocomplete.excludeLowerPriority (atom-specific)
  -- psciCommand (~atom-specific)
  -- buildOnSave

  pure $ toForeign $ fromFoldable $ unchangedOpts <> autocompleteOpts <> renamedOpts

config :: Foreign
config = toForeign
  { pscSourceGlob:
    { title: "PureScript source glob"
    , description: "Source glob to use to find .purs source files. Currently used for psc-ide-server to power goto-definition. (Requires restart/server restart command)"
    , type: "array"
    , default: defaultSrcGlob
    , items:
      { type: "string"
      }
    }
  , addPscPackageSources:
    { title: "Add psc-package sources"
    , description: "Whether to add psc-package sources to the globs passed to the IDE server for source locations (specifically the output of `psc-package sources`, if this is a psc-package project). Update due to adding packages/changing package set requires psc-ide server restart."
    , type: "boolean"
    , default: false
    }
  , pscIdeServerExe:
    { title: "psc-ide-server executable location"
    , description: "The location of the legacy `psc-ide-server` executable. May be on the PATH. (Requires restart/server restart command)"
    , type: "string"
    , default: "psc-ide-server"
    }
  , pursExe:
    { title: "purs location"
    , description: "The location of the combined `purs` executable. May be on the PATH. (Requires restart/server restart command)"
    , type: "string"
    , default: "purs"
    }
  , useCombinedExe:
    { title: "Use combined executable"
    , description: "Whether to use the new combined purs executable. This will default to true in the future then go away."
    , type: "boolean"
    , default: usePursDefault
    }
  , addNpmPath:
    { title: "Use npm bin directory"
    , description: "Whether to add the local npm bin directory to the PATH (e.g. to use locally installed purs/psc-ide-server if available). (Requires restart/server restart command)"
    , type: "boolean"
    , default: false
    }
  , editorMode:
    { title: "Editor mode"
    , description: "Set the editor-mode flag on the IDE server"
    , type: "boolean"
    , default: false
    }
  , polling:
    { title: "Polling mode"
    , description: "Set the polling flag on the IDE server"
    , type: "boolean"
    , default: false
    }
  , pscIdelogLevel:
    { title: "IDE server log level"
    , description: "Set the log-level of the IDE server: all | none | debug | perf."
    , type: "string"
    , default: "none"
    }
  , autoStartPscIde:
    { title: "Auto-start IDE server"
    , description: "Whether to automatically start IDE server. Otherwise the start command can be used."
    , type: "boolean"
    , default: true
    }
  , pscIdePort:
    { title: "IDE server port"
    , description: "Port to use for purs IDE server (whether an existing server or to start a new one). By default a random port is chosen (or an existing port in .psc-ide-port if present), if this is specified no attempt will be made to select an alternative port on failure."
    , type: [ "integer", "null" ]
    , default: toNullable Nothing
    }
  , buildCommand:
    { title: "Build command"
    , description: "Command line to build the project. "
        <> "Could be pulp (default), psc or a gulpfile, so long as it passes through errors from psc. "
        <> "Should output json errors (`--json-errors` flag). "
        <> "This is not interpreted via a shell, arguments can be specified but don't use shell features or a command with spaces in its path."
        <> "See [examples on the README](https://github.com/nwolverson/atom-ide-purescript/#build-configuration-hints)"
    , type: "string"
    , default: pulpCmd <> " build -- --json-errors"
    }
  , fastRebuild:
    { title: "Use fast rebuild"
    , description: "Use psc-ide-server rebuild function to build the current file only on save"
    , type: "boolean"
    , default: true
    }
  , censorWarnings:
    { title: "Censor warnings"
    , description: "The warning codes to censor, both for fast rebuild and a full build. Unrelated to any psa setup. e.g.: ShadowedName,MissingTypeDeclaration"
    , type: "array"
    , default: []
    , items:
      { type: "string"
      }
    }
  , psciCommand:
    { title: "psci command (eg 'psci' or 'pulp psci' or full path)"
    , description: "Command line to use to launch PSCI for the repl buffer. "
        <> "This is not interpreted via a shell, arguments can be specified but don't use shell features or a command with spaces in its path."
    , type: "string"
    , default: pulpCmd <> " psci"
    }
  , autocomplete:
    { type: "object"
    , properties:
      { addImport:
        { title: "Add import on autocomplete"
        , description: "Whether to automatically add imported identifiers when accepting autocomplete result."
        , type: "boolean"
        , default: true
        }
      , allModules:
        { title: "Suggest from all modules"
        , description: "Whether to always autocomplete from all built modules, or just those imported in the file. Suggestions from all modules always available by explicitly triggering autocomplete."
        , type: "boolean"
        , default: true
        }
      , limit:
        { title: "Result limit"
        , description: "Maximum number of results to fetch for an autocompletion request. May improve performance on large projects."
        , type: "integer"
        , default: 1000
        }
      , grouped:
        { title: "Group results"
        , type: "boolean"
        , default: true
        , description: "Whether to group completions in autocomplete results. Requires compiler 0.11.6"
        }
      , preferredModules:
        { type: "array"
        , default: ["Prelude"]
        , description: "Module to prefer to insert when adding imports which have been re-exported. In order of preference, most preferred first."
        , items:
          { type: "string"
          }
        }
      , excludeLowerPriority:
        { title: "Exclude other lower-priority providers"
        , description: "Whether to set the excludeLowerPriority flag for autocomplete+: disable this to see plain-text suggestions from context, for example. (Requires restart)"
        , type: "boolean"
        , default: true
        }
      }
    }
  }
