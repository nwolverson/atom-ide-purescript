LinterPurescript = require './linter-purescript'
PscIde = require './psc-ide'
PsTooltips = require './tooltips'
glob = require 'glob'
Editors = require './editors'
Psci = require './psci'
Pursuit = require './pursuit'

{ CompositeDisposable } = require 'atom'
{ ModuleSelectListView } = require('./select-views')

class Main
  config:
    pscIdePort:
      title: "psc-ide port number"
      type: 'integer'
      default: 4242
    pscIdeServerExe:
      title: "psc-ide-server executable location"
      type: 'string'
      default: process.env.HOME + '/.local/bin/psc-ide-server'
    buildCommand:
      title: "build command"
      description: "Command line to build the project. Could be pulp (default), psc or a gulpfile, so long as it passes through errors from psc. Should output json errors (old format will still be supported for now). Don't use a command with spaces in its path."
      type: 'string'
      default: if process.platform == "win32" then "pulp.cmd build --json-errors" else "pulp build --json-errors"
    buildOnSave:
      title: "build on save"
      description: "Build automatically on save. Enables in-line and collected errors. Otherwise a build command is available to be invoked manually."
      type: 'boolean'
      default: true
    psciCommand:
      title: "psci command (eg 'psci' or 'pulp psci' or full path)"
      description: "Command line to use to launch PSCI for the repl buffer. Don't use a command with spaces in its path."
      type: 'string'
      default: "pulp psci"

  constructor: ->
    @subscriptions = new CompositeDisposable()

  activate: (state) =>
    console.log "Activated ide-purescript"
    @pscide = new PscIde()
    @editors = new Editors()
    @tooltips = new PsTooltips(@pscide)
    @pursuit = new Pursuit(@pscide, @editors)
    @pscide.activate(@editors)
      .then () =>
        @tooltips.activate()
    @psci = new Psci()
    @psci.activate()
    atom.commands.add("atom-workspace", "purescript:pursuit-search", @pursuit.search)
    atom.commands.add("atom-workspace", "purescript:pursuit-search-modules", @pursuit.searchModule)
    atom.commands.add("atom-workspace", "purescript:build", @lint)
    atom.commands.add("atom-workspace", "purescript:show-quick-fixes", @quickfix)

    atom.commands.add("atom-workspace", "purescript:add-module-import", =>
      moduleView = new ModuleSelectListView(@editors)
      moduleView.show()
      )

    @subscriptions.add @editors
    @subscriptions.add @tooltips
    @subscriptions.add @pscide

  deactivate: () =>
    @subscriptions.dispose()

  provideAutocomplete: =>
    selector: '.source.purescript'
    disableForSelector: '.source.purescript .comment, .source.purescript .string'
    inclusionPriority: 1
    excludeLowerPriority: true
    getSuggestions: @pscide.getSuggestions
    # onDidInsertSuggestion: ({editor, triggerPosition, suggestion}) ->
    # dispose: ->

  lint: () =>
    @pslinter.lintOnBuild()

  quickfix: () =>
    @editors.showQuickFixes()

  consumeLinterIndie: (linterRegistry) =>
    linter = linterRegistry.register({name: 'PureScript'})
    @subscriptions.add linter
    @pslinter = new LinterPurescript(@editors, linter)
    @editors.linter = @pslinter

  consumeLinterInternal: (linterMain) =>
    @editors.linterMain = linterMain


module.exports = new Main()
