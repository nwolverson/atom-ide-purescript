# linter-purescript package

This linter plugin for [Linter](https://github.com/AtomLinter/Linter) provides an interface to errors and warnings from the PureScript compiler, by running `pulp build` on the project (on save). It will be used with files that have the `PureScript` syntax.

Probably not good for actual use due to lack of configuration and speed of compilation. Better to be on explicit command than
save (like [Build](https://atom.io/packages/build) - see [linter support](https://github.com/noseglid/atom-build/pull/117) PR).

## Installation
Linter package must be installed in order to use this plugin. If Linter is not installed, please follow the instructions [here](https://github.com/AtomLinter/Linter).
