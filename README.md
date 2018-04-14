# ide-purescript package for Atom

[![Build Status](https://travis-ci.org/nwolverson/atom-ide-purescript.svg?branch=master)](https://travis-ci.org/nwolverson/atom-ide-purescript)

This package provides editor support for PureScript projects in Atom, based on a [Language Server](https://github.com/nwolverson/purescript-language-server) which uses the `purs` compiler IDE server functionality,
with UI provided via the atom-ide framework.

This package provides:
  * [Autocomplete](#autocomplete)
  * [Tooltips](#tooltips)
  * [Go-to-definition](#go-to-definition)
  * [Pursuit lookup](#pursuit-lookup) and local search
  * [PSCI](#psci)
  * [Build](#build) and error reporting
  * [Quick-Fix](#error-suggestions--quick-fix)
  * [Case-Split](#case-split--add-clause)

Package should trigger on opening a `.purs` file or running any PureScript/PSCI command from the menu bar or command palette. For full support of atom-ide features, your PureScript project should be opened at the root folder level in Atom.

## Installation and General Use

For best results (and default settings) install dependencies:

  * `purs` (compiler) 0.8.5 or >= 0.9.1 (includes `psc-ide-server`/ `ide server` subcommand as standard)
    * 0.9.2 required for go-to definition, 0.9.x for some compiler suggestions
  * `pulp` >= 8.0.0 (appropriate for your `psc` version). *>=10.0.0 recommended* to use default build command

Required atom packages - these should be auto-installed by starting the package, eg by running any PureScript command.
  * [language-purescript](https://atom.io/packages/language-purescript)
  * [atom-ide-ui](https://github.com/facebook-atom/atom-ide-ui/)

For use with older versions of pulp, or for alternative build tools and configuration tips, [see below](#build). In brief
the build command is configurable, but should output JSON errors.

## IDE server

This package runs a langauge server process, which itself starts a compiler server process, `purs ide server`
(previously `psc-ide-server`), to provide type information, completions,etc. This package will automatically start
the IDE server in your project directory and kill it when closing.

You can start/stop and restart this via provided commands, and configure the server to auto-start or not as desired; if for some reason if for some reason
you want to manually start a `purs ide server` process you can start that before starting atom, and configure the server port to match (but beware clashes of multiple instances, and the path must match).

Multiple PureScript projects are supported, by adding each project as a top level folder.

For all functions provided by `purs ide server` you will need to build your project first! In particular a full build, not a "fast-build" on save, is required first time or after upgrading `purs`, afterwards saving individual files will update tooltips etc.

## Autocomplete

Provided from the IDE server. Make sure your project is built first. Only for top level definitions.

Completions will be sourced from all available modules by default; this is configurable to just those imported in the current file, in which case explicitly (re-)triggering the completion will expand to show all modules.

Imports will be inserted automatically for completions! Again this is configurable.

## Tooltips

Hovering over an identifier will show a tooltip with its type (only for top level definitions):

![Type tooltip](http://nwolverson.github.io/atom-ide-purescript/assets/type-tooltip.png)

This is really stupid, and only cares that you hover over a word regardless of context, you will get some false positives.

## Go to definitions

Hyperclick goto-definition functionality is supported. This is available with `purs` version
0.9.2 and above, and like tooltips/autocomplete works on identifiers bound at the top level.
This is available as command/ctrl+click or cmd+alt/opt+enter by default, see atom-ide-ui config.

In case source positions are not up to date, they may not be updated on rebuild, try rebuilding or restarting psc-ide server.

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

As well as this there is "fast rebuild" via the IDE server on save (by default), this will
build an individual file. The recommended approach is to run a full build initially and
after any dependency upgrades, compiler updates, etc. or when producing build artifacts, and
otherwise quick build for continuous error feedback.

### Build configuration hints

The default build command is
```
pulp build -- --json-errors
```
(on windows `pulp.cmd` is called instead). *Note* This default requires pulp 0.10 as command syntax changed to require `--` separator.
This is configurable: the command should be on your PATH (or could be an explicit absolute path) with arguments, such that it will
output JSON errors as per `psc`, on stderr. This is *not* interpreted via shell, simply
pulled apart as a list of string separated arguments.

Some alternatives:
  * Pulp with `psc-package`: `pulp --psc-package build -- --json-errors`
  * Direct `psc` use: `psc bower_components/purescript-*/src/**/*.purs src/**/*.purs --json-errors`
  * Run a `purescript-gulp` based build: `gulp` - again need to ensure this outputs JSON errors, you probably want a specific task for this.
  * Pulp passing through `psa`: `pulp build -- --stash --json-errors`

    This will pass through `psc` errors as JSON but also integrate to any external `psa` stash,
    e.g. if you're running `psa` on a terminal somewhere. Right now the stashed warnings are not exposed in the JSON.
  * Ensure tests are compiled in the build: `pulp build --include test -- --json-errors`
  * Via npm run script: `npm run -s build`. Or if the run script does not output json errors you might be able to pass
    an extra flag: `npm run -s build -- --json-errors` - more information [on this issue](https://github.com/nwolverson/atom-ide-purescript/issues/53#issuecomment-198621810).

Since atom unfortunately does not support per-project configuration, the npm run script approach may be particularly
useful where you have different projects that build differently. Alternatively you can look into
the [project-manager](https://atom.io/packages/project-manager) package.

You may be able to get away without thinking about all this if your project specific setup is only required for a "full" build
(e.g. browserify step) and not just for the basic compilation stage.

## Error Suggestions / Quick-Fix

![Error indication](https://user-images.githubusercontent.com/2770891/38769617-a3e6ad8c-3ffd-11e8-8874-68bc567fc520.png)

![Error fix UI](https://user-images.githubusercontent.com/2770891/38769616-a3cd0364-3ffd-11e8-99f0-13f2e7ed517c.png)

Error suggestions may be triggered from some underlined compiler warnings. Suggestions apply on hover, or
a context menu can be triggered by 'alt-A' (Diagnostics: Show Actions at Position) when the cursor is on an eligible warning. Currently fixable warnings:

  * Unused or duplicate import (`UnusedImport`, `DuplicateImport`)
  * Extraneous identifiers in explicit import list (`UnusedExplicitImport`)
  * Implicit imports that should be made explicit - this will apply to all but remember that 1 open import is allowed without warning (`ImplicitImport`, `ImplicitQualifiedImport`, `HidingImport`)
  * Missing top level type declaration - add type signature. Note you will need to quantify any type variables that appear,
  and maybe re-qualify identifiers (`MissingTypeDeclaration`)
  * Inferred type for wildcard `_` - fill in type signature. Notes as above. (`WildcardInferredType`)

## Case split / add clause

![Case split/add clause](https://cloud.githubusercontent.com/assets/2770891/17080720/2c5391d8-5132-11e6-8bf3-0a7e0714da24.gif)

  * **Add clause** (default binding: `^O c`) - use psc-ide to add a clause to the current top-level function (from cursor on its type definition).
  * **Case split** (default binding: `^O s`) - with cursor on an function argument identifier, add clauses to the definition to case-split on that argument.

Currently the type of the arguments must be provided by the user.

## Hacking on atom-ide-purescript

After cloning, install dependencies
  • `psc-package update`
  • `npm install`

Bundle for Atom: `npm run -s bundle`

You can use the regular `pulp build` as part of your tooling or to see compile errors, but bundling is required for the plugin to be usable by Atom and will build the project as part of the task.

To use your local development version, you will first need to uninstall any current version you have installed. Then from within the atom-ide-purescript directory run `apm link`. This will create a symlink from the Atom plugins directory to your development directory.
