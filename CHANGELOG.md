## 0.24.0

* Updates from `purescript-language-server` version `0.11.0`:
  - Add find references command (requires purs 0.12). Currently works at the value level

## 0.23.1

* List `pscIdePort` config on config page

## 0.23.0

* Updates from `purescript-language-server` version `0.10.2`
  - 0.10.2
    - Add warning/build option on missing output directory - https://github.com/nwolverson/purescript-language-server/commit/83e7f2b884915100318bb6a06eb5b59fd7e39354
  - 0.10.1
    - Respect `pscIdePort` config - when absent port will be auto-chosen, when present server will be found or started on that port
    - Respect `autoStartPscIde` config
    - Make `executeCommandProvider` optional
    - `fixTypo` position fix
  - 0.10.0
    - Replace typed hole command & code action (requires LSP client support) https://github.com/nwolverson/purescript-language-server/issues/14
      - *Not supported in atom-ide-purescript*
    - Move dependencies from purescript-ide-purescript-core
    - Configurable output directory #30
    - Fix all suggestions commands https://github.com/nwolverson/purescript-language-server/issues/12
  - 0.9.0
    - Add documentation to hover tooltips https://github.com/nwolverson/purescript-language-server/pull/25 [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
    - Make compiler fixes (particularly import fixes) not leave extra blank lines https://github.com/nwolverson/purescript-language-server/issues/13
    - Fix `preludeModule` adding a prelude import if it is already imported explicitly https://github.com/nwolverson/purescript-language-server/issues/26
    - Ensure IDE server dependencies are reloaded on full build (particularly in case of editor mode) https://github.com/nwolverson/purescript-language-server/issues/19
    - Fix completion edits in some circumstances https://github.com/nwolverson/vscode-ide-purescript/issues/96
  - 0.8.0
    - Add suggestion ranking heuristics, currently these are for qualified import suggestions https://github.com/nwolverson/purescript-language-server/pull/15 [@natefaubion](https://github.com/natefaubion)
      - *Not properly supported in atom-ide-purescript*
    - Add configurable Prelude open import via `preludeModule` https://github.com/nwolverson/purescript-language-server/pull/16 [@natefaubion](https://github.com/natefaubion)
    - Use Markdown for suggestion details https://github.com/nwolverson/purescript-language-server/pull/23 [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

* Fix issues with starting/stopping language servers & command execution with multiple project folders [#190](https://github.com/nwolverson/atom-ide-purescript/issues/190). Note that due to atom-languageclient design currently the IDE server will be stopped on closing the last project file in the directory, and started fresh on opening a new one.

## 0.22.0

* Updates from `purescript-language-server` version `0.7.1`
  - Automatically add qualified imports on completion with an unknown qualifier https://github.com/nwolverson/purescript-language-server/pull/7 [@natefaubion](https://github.com/natefaubion)
  - Fix extraneous newlines in case split/add clause https://github.com/nwolverson/purescript-language-server/pull/9

## 0.21.0

* Updates from `purescript-language-server` version `0.6.0`:
  - Support psc-package source globs. Toggled via `addPscPackageSources` config (default `false`) and using `psc-package sources` command.
  - Show expanded type in tooltips (when different from non-expanded one)
  - Show module tooltip for qualified imports (hover over the qualifier of a qualified identifier)

## 0.20.0

* Switch to using purescript-language-server

## ... changelog unmaintained

## 0.1.0 - First Release
* Kind of works for me (OSX). Needs config.
