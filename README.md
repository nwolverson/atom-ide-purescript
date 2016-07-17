# ide-purescript package for Atom

This package provides editor support for PureScript projects in Atom. Dependency [language-purescript](https://atom.io/packages/language-purescript) provides syntax highlighting.

This package provides:
  * Build and error reporting
  * Autocompletion
  * Type info tooltips

Package should trigger on opening a `.purs` file or running any PureScript/PSCI command from the menu bar or command palette.

## Installation and General Use

For best results (and default settings) install dependencies:

  * `psc` 0.8.5 or >= 0.9.1 (includes `psc-ide-server` as standard)
    * 0.9.2 required for go-to definition, 0.9.x for some compiler suggestions
  * `pulp` >= 8.0.0 (appropriate for your `psc` version)

Required atom packages - these should be auto-installed by starting the package, eg by running any PureScript command.
  * [language-purescript](https://atom.io/packages/language-purescript)
  * [Linter](https://github.com/atom-community/linter) are required,

Optional atom packages:

  * [hyperclick](https://atom.io/packages/hyperclick) - click to go to definition (this package contains a hyperclick provider)

For use with older versions of pulp, or for alternative build tools and configuration tips, [see below](#build). In brief
the build command is configurable, but should output JSON errors.

## psc-ide

This package runs a server process, `psc-ide-server`, to provide type information, completions,
etc. This package will automatically start `psc-ide-server` in your project
directory (port is configurable) and kill it when closing, if for some reason
you want a longer running server process you should be able to start that before
starting atom.

Multiple PureScript projects are now supported, whether in one or multiple Atom windows, see [release notes](https://github.com/nwolverson/atom-ide-purescript/releases/tag/v0.14.0) for details.

Note `psc-ide-client` is not used.

For all functions provided by `psc-ide` you will need to build your project first! In particular a full build,
not a "fast-build" on save, is required first time or after upgrading psc, afterwards saving individual files
will update tooltips etc.

## Autocomplete

Provided from `psc-ide`. Make sure your project is built first. Only for top level definitions.

Completions will be sourced from all available modules by default; this is configurable to just those imported in the current file, in which case explicitly (re-)triggering the completion will expand to show all modules.

Imports will be inserted automatically for completions! Again this is configurable.

## Tooltips

Hovering over an identifier will show a tooltip with its type (only for top level definitions):

![Type tooltip](http://nwolverson.github.io/atom-ide-purescript/assets/type-tooltip.png)

This is really stupid, and only cares that you hover over a word regardless of context, you will get some false positives.

## Pursuit lookup

![Pursuit lookup](http://nwolverson.github.io/atom-ide-purescript/assets/pursuit.png)

Command available from the command palette:
  * PureScript search - search for identifiers, by identifier or type
  * PureScript search modules - find package by module
  * PureScript search - a local search of identifiers from built modules

## PSCI

![PSCI window](http://nwolverson.github.io/atom-ide-purescript/assets/psci.png)

Basic PSCI REPL integration (runs `pulp psci`). Comprises a read-only pane which displays
PSCI output, and an input field to send expressions to the REPL (just hitting enter). Input
can be sent from the current buffer by line or selection.

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

## Case split / add clause

*EXPERIMENTAL*

Add clause - use psc-ide to add a clause to the current top-level function (from cursor on its type definition).
Case split - with cursor on an function argument identifier, add clauses to the definition to case-split on that argument.


## Hacking on atom-ide-purescript

After cloning, install dependencies
  • `bower install`
  • `npm install`

Bundle for Atom: `npm run bundle`
_Alternatively `npm run -s bundle` if you want cleaner output_

You can use the regular `pulp build` as part of your tooling or to see compile errors, but bundling is required for the plugin to be usable by Atom and will build the project as part of the task.

To use your local development version, you will first need to uninstall any current version you have installed. Then from within the atom-ide-purescript directory run `apm link`. This will create a symlink from the Atom plugins directory to your development directory.
