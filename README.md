# ide-purescript package for Atom

This package provides editor support for PureScript projects in Atom. You should
first install required dependency [language-purescript](https://atom.io/packages/language-purescript)
which also provides syntax highlighting.

This package provides:
  * Build and error reporting
  * Autocompletion
  * Type info tooltips

Package should trigger on opening a `.purs` file.

## Installation and General Use

Atom packages:

  * You *must* install the atom package [language-purescript](https://atom.io/packages/language-purescript)! Otherwise nothing will happen.
  * It is strongly suggested to install atom packages [Linter](https://github.com/atom-community/linter) for build support

For best results (and default settings) install dependencies:

  * `psc` >= 0.8.0
  * `pulp` >= 8.0.0

For use with older versions of pulp, or for alternative build tools and configuration tips, [see below](#build). In brief
the build command is configurable, but should output JSON errors.

## psc-ide

This package runs a server process, `psc-ide-server`, to provide type information, completions,
etc. This package will automatically start `psc-ide-server` in your project
directory (port is configurable) and kill it when closing, if for some reason
you want a longer running server process you should be able to start that before
starting atom. Multiple projects currently not supported, but you should be able
to use the [project-manager](https://atom.io/packages/project-manager) package.

Note `psc-ide-client` is not used.

For all functions provided by `psc-ide` you will need to build your project first!

## Autocomplete

Provided from `psc-ide`. Make sure your project is built first.

Completions will be sourced from modules imported in the current file.

## Tooltips

Hovering over an identifier will show a tooltip with its type:

![Type tooltip](http://nwolverson.github.io/atom-ide-purescript/assets/type-tooltip.png)

This is really stupid, and only cares that you hover over a word regardless of context, you will get some false positives.

## Pursuit lookup

![Pursuit lookup](http://nwolverson.github.io/atom-ide-purescript/assets/pursuit.png)

Command available from the command palette:
  * PureScript search - search for identifiers, by identifier or type
  * PureScript search modules - find package by module

## PSCI

![PSCI window](http://nwolverson.github.io/atom-ide-purescript/assets/psci.png)

Basic PSCI REPL integration (runs `pulp psci`). A read-only buffer which displays
PSCI output, input can be sent from the current buffer by line or selection.

Command from the command palette:
  * PSCI: Open - open a PSCI buffer
  * PSCI: Send Line
  * PSCI: Send Selection

## Build

Build support is provided via `pulp build` by default, configurable to any command which
will output psc errors. This can be configured to run on save, alternatively there
is a 'PureScript Build' command.

### Build configuration hints

The default build command is
```
pulp build --no-psa --json-errors
```
(on windows `pulp.cmd` is called instead). This is configurable: the command should be
on your PATH (or could be an explicit absolute path) with arguments, such that it will
output JSON errors as per `psc`, on stderr. This is *not* interpreted via shell, simply
pulled apart as a list of string separated arguments.

Some alternatives:
  * Direct `psc` use: `psc bower_components/*/src/**/*.purs bower_components/*/src/**/*.js src/**/*.purs src/**/*.js --json-errors`
  * Run a `purescript-gulp` based build: `gulp` - again need to ensure this outputs JSON errors, you probably want a specific task for this.
  * Pulp passing through `psa`: `pulp build --stash --json-errors`

    This will pass through `psc` errors as JSON but also integrate to any external `psa` stash,
    e.g. if you're running `psa` on a terminal somewhere. Right now the stashed warnings are not exposed in the JSON.
  * Ensure tests are compiled in the build: `pulp build --include test --json-errors`
  * Via npm run script: `npm run -s build`. Or if the run script does not output json errors you might be able to pass
    an extra flag: `npm run -s build -- --json-errors` - more information [on this issue](https://github.com/nwolverson/atom-ide-purescript/issues/53#issuecomment-198621810).

Since atom unfortunately does not support per-project configuration, the npm run script approach may be particularly
useful where you have different projects that build differently. Alternatively you can look into
the [project-manager](https://atom.io/packages/project-manager) package.

You may be able to get away without thinking about all this if your project specific setup is only required for a "full" build
(e.g. browserify step) and not just for the basic compilation stage.


## Error Suggestions

![Error suggestions](https://cloud.githubusercontent.com/assets/2770891/12066635/d6b14964-afe2-11e5-8584-44d291044614.gif)

Error suggestions may be triggered from some underlined compiler warnings. There
is no visual indication, currently this will basically be for 'import' warnings,
and can be triggered by 'alt-enter' (PureScript: Show Quick Fixes).

## Hacking on atom-ide-purescript

After cloning, install dependencies
  • `bower install`
  • `npm install`

Bundle for Atom: `npm run bundle`
_Alternatively `npm run -s bundle` if you want cleaner output_

You can use the regular `plup build` as part of your tooling or to see compile errors, but bundling is required for the plugin to be usable by Atom and will build the project as part of the task.

To use your local development version, you will first need to uninstall any current version you have installed. Then from within the atom-ide-purescript directory run `apm link`. This will create a symlink from the Atom plugins directory to your development directory.
