module IdePurescript.Atom.Config (config) where

import Prelude((++))

pulpCmd :: String
pulpCmd = "pulp" -- pulpCmd = if process.platform == 'win32' then 'pulp.cmd' else 'pulp'

config =
  { pscIdePort:
    { title: "psc-ide port number"
    , type: "integer"
    , default: 4242
    }
  , pscIdeServerExe:
    { title: "psc-ide-server executable location"
    , type: "string"
    , default: "psc-ide-server"
    }
  , buildCommand:
    { title: "build command"
    , description: "Command line to build the project. Could be pulp (default), psc or a gulpfile, so long as it passes through errors from psc. Should output json errors (old format will still be supported for now). Don't use a command with spaces in its path."
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
    , description: "Command line to use to launch PSCI for the repl buffer. Don't use a command with spaces in its path."
    , type: "string"
    , default: pulpCmd ++ " psci"
    }
  }
