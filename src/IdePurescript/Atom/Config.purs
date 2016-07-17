module IdePurescript.Atom.Config (config, getSrcGlob) where

import Prelude
import Node.Process as P
import Atom.Atom (getAtom)
import Atom.Config (CONFIG, getConfig)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (mapMaybe)
import Data.Bifunctor (rmap)
import Data.Either (either)
import Data.Foreign (readString, readArray, Foreign, toForeign)
import Data.Maybe (Maybe(..))
import Node.Platform (Platform(Win32))

defaultSrcGlob :: Array String
defaultSrcGlob = ["src/**/*.purs", "bower_components/**/*.purs"]

getSrcGlob :: forall eff. Eff (config :: CONFIG | eff) (Array String)
getSrcGlob = do
  atom <- getAtom
  srcGlob <- liftEff $ readArray <$> getConfig atom.config "ide-purescript.pscSourceGlob"
  let srcGlob' = rmap (mapMaybe $ (either (const Nothing) Just) <<< readString) srcGlob
  pure $ either (const defaultSrcGlob) id srcGlob'

pulpCmd :: String
pulpCmd = if P.platform == Win32 then "pulp.cmd" else "pulp"

config :: Foreign
config = toForeign
  { pscSourceGlob:
    { title: "PureScript source glob"
    , description: "Source glob to use to find .purs source files. Currently used for psc-ide-server to power goto-definition."
    , type: "array"
    , default: defaultSrcGlob
    , items:
      { type: "string"
      }
    }
  , pscIdeServerExe:
    { title: "psc-ide-server executable location"
    , description: "The location of the `psc-ide-server` executable. Note this is *not* `psc-ide-client`. May be on the PATH."
    , type: "string"
    , default: "psc-ide-server"
    }
  , buildCommand:
    { title: "Build command"
    , description: "Command line to build the project. "
        <> "Could be pulp (default), psc or a gulpfile, so long as it passes through errors from psc. "
        <> "Should output json errors (`--json-errors` flag). "
        <> "This is not interpreted via a shell, arguments can be specified but don't use shell features or a command with spaces in its path."
        <> "See [examples on the README](https://github.com/nwolverson/atom-ide-purescript/#build-configuration-hints)"
    , type: "string"
    , default: pulpCmd <> " build --no-psa --json-errors"
    }
  , buildOnSave:
    { title: "Build on save"
    , description: "Build automatically on save. Enables in-line and collected errors. Otherwise a build command is available to be invoked manually."
    , type: "boolean"
    , default: true
    }
  , fastRebuild:
    { title: "Use fast rebuild"
    , description: "Use psc-ide-server rebuild function to build the current file only on save"
    , type: "boolean"
    , default: true
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
      }
    }
  }
