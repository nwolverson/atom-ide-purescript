LinterPurescript = require './linter-purescript'
PscIde = require './psc-ide'

glob = require("glob")

module.exports =
  config:
    pscIdeExe:
      title: "psc-ide executable location"
      type: 'string'
      default: process.env.HOME + '/.local/bin/psc-ide'

  activate: (state) ->
    console.log "Activated ide-purescript"
    @pscide = new PscIde()

  provideAutocomplete: ->
    selector: '.source.purescript'
    inclusionPriority: 1
    excludeLowerPriority: true
    getSuggestions: @pscide.getSuggestions
    # onDidInsertSuggestion: ({editor, triggerPosition, suggestion}) ->
    # dispose: ->


  provideLinter: ->
    linter = new LinterPurescript()
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
        files = glob.sync("src/**/*.purs", {cwd: path})
        files && files.length > 0
      settings: (path) ->
        return new Promise (resolve,reject) =>
          resolve {
            cmd: 'pulp'
            exec: 'pulp'
            sh: true
            args: ['build']
            errorMatch: [
              '(?<type>Error|Warning) at (?<file>[^\n]*?) line (?<line>[0-9]+), column (?<col>[0-9]+)',
              'Unable to parse module:\n[^"]*"(?<file>[^"]+)" \\(line (?<line>[0-9]+), column (?<col>[0-9]+)\\):'
            ]
          }
    }
