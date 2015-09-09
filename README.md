# ide-purescript package for Atom

This package provides editor support for PureScript projects in Atom. You should
first install required dependency [language-purescript](https://atom.io/packages/language-purescript)
which also provides syntax highlighting and [psc-ide](https://github.com/kRITZCREEK/psc-ide) (see below).

This package provides:
  * Build and error reporting
  * Autocompletion
  * Type info tooltips

Package should trigger on opening a `.purs` file.

## Autocomplete

Provided from [psc-ide](https://github.com/kRITZCREEK/psc-ide).

1. Start `psc-ide-server` in your project directory
1. Configure `psc-ide` path for the package
1. Actually build your project
1. Load the modules you want to use as completion sources: `echo "dependencies My.Project.Module" | psc-ide`
1. Edit a `.purs` file and you should see completion.

## Tooltips

Hovering over an identifier will show a tooltip with its type:

![Type tooltip](http://nwolverson.github.io/atom-ide-purescript/assets/type-tooltip.png)

This is really stupid, and only cares that you hover over a word regardless of context, you will get some false positives.

## Custom port

To run `psc-ide-server` on a custom port, start the command
with the `-p` flag, e.g. `psd-ide-server -p 4243`. Then
set the same number in the package settings.

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
