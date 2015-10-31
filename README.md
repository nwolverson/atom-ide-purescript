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

This package relies on having [psc-ide](https://github.com/kRITZCREEK/psc-ide) installed.
For use with PureScript compiler version *0.7.5* you should use [version 0.4.0](https://github.com/kRITZCREEK/psc-ide/releases/tag/0.4.0),
for earlier compiler versions you instead need [0.3.0.0](https://github.com/kRITZCREEK/psc-ide/releases/tag/0.3.0.0).
This runs a server process, `psc-ide-server`, to provide type information, completions,
etc. This package will automatically start `psc-ide-server` in your project
directory (port is configurable) and kill it when closing, if for some reason
you want a longer running server process you should be able to start that before
starting atom. *Multiple projects currently not supported!*

For all functions provided by `psc-ide` you will need to build your project first!
Dependencies will automatically be loaded via `dependencies Current.File` as
required.

You *must* install the atom package [language-purescript](https://atom.io/packages/language-purescript)!
You should optionally install atom packages [Linter](https://github.com/AtomLinter/Linter)
or [Build](https://atom.io/packages/build) if you want build support, see below.

The package will start on opening a `.purs` file, note there is currently an Atom bug
relating to starting a session with already open files.

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

Build support is experimental as I don't like any of the options yet. Depending on
which other packages you have installed, this package provides (via `pulp build`)

  * [Linter](https://github.com/AtomLinter/Linter) support - a bit annoying since
  compile on save is too slow, but some nice messages

  * [Build](https://atom.io/packages/build) support - will only work properly in
  PureScript projects if you don't have Gulpfile etc., as `pulp` is always used
  but custom providers are at the bottom of the priority list.

Might be that something like [Build linter support](https://github.com/noseglid/atom-build/pull/117) could
be useful, or build triggered from this package.
