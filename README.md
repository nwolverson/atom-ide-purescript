# ide-purescript package for Atom

This package provides editor support for PureScript projects in Atom. You should
first install required dependency [language-purescript](https://atom.io/packages/language-purescript)
which also provides syntax highlighting and [psc-ide](https://github.com/kRITZCREEK/psc-ide) (see below).

This package provides:
  * Build and error reporting
  * Autocompletion
  * Type info tooltips

Package should trigger on opening a `.purs` file.

## Installation and General Use

Atom packages:

  * You *must* install the atom package [language-purescript](https://atom.io/packages/language-purescript)! Otherwise nothing will happen.
  * It is strongly suggested to install atom packages [Linter](https://github.com/AtomLinter/Linter) for build support

For best results (and default settings) install dependencies:

  * `psc` >= 0.8.0
  * [psc-ide](https://github.com/kRITZCREEK/psc-ide) >= 0.6.0
  * `pulp` >= 8.0.0

For use with older versions of the PureScript compiler, check
[psc-ide](https://github.com/kRITZCREEK/psc-ide) documentation for the required
version, and change default build command.

For use with older versions of pulp, or for alternative build tools, change the
default build command in package settings. For best results ensure JSON compiler
errors are output on stderr.

## psc-ide

This package runs a server process, `psc-ide-server`, to provide type information, completions,
etc. This package will automatically start `psc-ide-server` in your project
directory (port is configurable) and kill it when closing, if for some reason
you want a longer running server process you should be able to start that before
starting atom. Multiple projects currently not supported, but you should be able
to use the [project-manager](https://atom.io/packages/project-manager) package.

For all functions provided by `psc-ide` you will need to build your project first!
Dependencies will automatically be loaded via `dependencies Current.File` as
required.

## Autocomplete

Provided from [psc-ide](https://github.com/kRITZCREEK/psc-ide). Make sure
your project is built first.

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

## Error Suggestions

Error suggestions may be triggered from some underlined compiler warnings. There
is no visual indication, currently this will basically be for 'import' warnings,
and can be triggered by 'alt-enter' (PureScript: Show Quick Fixes).
