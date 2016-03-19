module IdePurescript.Atom.Config (config) where

import Prelude((++),(==))
import Node.Process as P
import Node.Platform (Platform(Win32))

pulpCmd :: String
pulpCmd = if P.platform == Win32 then "pulp.cmd" else "pulp"

config =
  { pscIdePort:
    { title: "psc-ide port number"
    , description: "The port to use to communciate with `psc-ide-server`, also to launch the server with if required. "
        ++ "The default port is 4242 and this only need be changed if you've explicitly chosen to use another port."
    , type: "integer"
    , default: 4242
    }
  , pscIdeServerExe:
    { title: "psc-ide-server executable location"
    , description: "The location of the `psc-ide-server` executable. Note this is *not* `psc-ide-client`. May be on the PATH."
    , type: "string"
    , default: "psc-ide-server"
    }
  , buildCommand:
    { title: "build command"
    , description: "Command line to build the project. "
        ++ "Could be pulp (default), psc or a gulpfile, so long as it passes through errors from psc. "
        ++ "Should output json errors (`--json-errors` flag). "
        ++ "This is not interpreted via a shell, arguments can be specified but don't use shell features or a command with spaces in its path."
        ++ "See [examples on the README](https://github.com/nwolverson/atom-ide-purescript/#build-configuration-hints)"
    , type: "string"
    , default: pulpCmd ++ " build --no-psa --json-errors"
    }
  , buildOnSave:
    { title: "build on save"
    , description: "Build automatically on save. Enables in-line and collected errors. Otherwise a build command is available to be invoked manually."
    , type: "boolean"
    , default: true
    }
  , psciCommand:
    { title: "psci command (eg 'psci' or 'pulp psci' or full path)"
    , description: "Command line to use to launch PSCI for the repl buffer. "
        ++ "This is not interpreted via a shell, arguments can be specified but don't use shell features or a command with spaces in its path."
    , type: "string"
    , default: pulpCmd ++ " psci"
    }
  }
