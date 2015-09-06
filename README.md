# ide-purescript package for Atom

This package provides editor support for PureScript projects in Atom. For basic
syntax highlighting you should install [language-purescript](https://atom.io/packages/language-purescript).

This package provides:
  * Build and error reporting
  * Autocompletion

Package should trigger on opening a `.purs` file.

## Autocomplete

1. Start `psc-ide-server` in your project directory
2. Configure `psc-ide` path for the package
3. Load the modules you want to use as completion sources: `echo "load Prelude" | psc-ide`
4. Edit a `.purs` file and you should see completion.

## Build

Build support is experimental as I don't like any of the options yet. Depending on
which other packages you have installed, this package provides (via `pulp build`)

  * [Linter](https://github.com/AtomLinter/Linter) support - a bit annoying since
  compile on save is too slow, but some nice messages

  * [Build](https://atom.io/packages/build) support - will only work properly in
  PureScript projects if you don't have Gulpfile etc., as `pulp` is always used
  but custom providers are at the bottom of the priority list.

Might be that something like Build [linter support](https://github.com/noseglid/atom-build/pull/117) PR) could
be useful, or build triggered from this package.
