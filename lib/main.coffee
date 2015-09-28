LinterPurescript = require './linter-purescript'
PscIde = require './psc-ide'
PsTooltips = require './tooltips'
glob = require 'glob'
Editors = require './editors'
Psci = require './psci'
Pursuit = require './pursuit'

module.exports =
  config:
    pscIdeExe:
      title: "psc-ide executable location"
      type: 'string'
      default: process.env.HOME + '/.local/bin/psc-ide'
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
      type: 'string'
      default: "pulp build"
    enableAtomLinter:
      title: "enable atom-linter (build on save, error tooltips)"
      type: 'boolean'
      default: true
    enableBuild:
      title: "enable build (atom build package)"
      type: 'boolean'
      default: true
    psciCommand:
      title: "psci command (eg 'psci' or 'pulp psci' or full path)"
      type: 'string'
      default: "pulp psci"

  activate: (state) ->
    console.log "Activated ide-purescript"
    @pscide = new PscIde()
    @editors = new Editors()
    @tooltips = new PsTooltips(@pscide)
    @pursuit = new Pursuit(@pscide)
    @pscide.activate(@editors)
      .then () =>
        @tooltips.activate()
    @psci = new Psci()
    @psci.activate()
    atom.commands.add("atom-workspace", "purescript:pursuit-search", @pursuit.search)
    atom.commands.add("atom-workspace", "purescript:pursuit-search-modules", @pursuit.searchModule)
    ModuleSelectListView = require('./select-views')

    atom.commands.add("atom-workspace", "purescript:add-module-import", =>
      moduleView = new ModuleSelectListView(@editors)
      moduleView.initialize()
      moduleView.show()
      )

  deactivate: () ->
    @editors.dispose()
    @tooltips.deactivate()
    @pscide.deactivate()

  provideAutocomplete: ->
    selector: '.source.purescript'
    disableForSelector: '.source.purescript .comment, .source.purescript .string'
    inclusionPriority: 1
    excludeLowerPriority: true
    getSuggestions: @pscide.getSuggestions
    # onDidInsertSuggestion: ({editor, triggerPosition, suggestion}) ->
    # dispose: ->


  provideLinter: ->
    linter = new LinterPurescript(@editors)
    return {
      grammarScopes: ['source.purescript']
      scope: 'package'
      lint: (f) -> linter.lint(f)
      lintOnFly: false
    }
  provideBuildConfig: ->
    return {
      niceName: 'PureScript',
      isEligable: (path) ->
        if !atom.config.get("ide-purescript.enableBuild")
          return false
        files = glob.sync("src/**/*.purs", {cwd: path})
        files && files.length > 0
      settings: (path) ->
        buildCommand = atom.config.get("ide-purescript.buildCommand").split(/\s+/)
        cmd = buildCommand[0]
        args = buildCommand.slice 1
        return new Promise (resolve,reject) =>
          resolve {
            cmd: cmd
            exec: cmd
            sh: true
            args: args
            errorMatch: [
              '(?<type>Error|Warning) at (?<file>[^\n]*?) line (?<line>[0-9]+), column (?<col>[0-9]+)',
              'Unable to parse module:\n[^"]*"(?<file>[^"]+)" \\(line (?<line>[0-9]+), column (?<col>[0-9]+)\\):'
            ]
          }
    }
